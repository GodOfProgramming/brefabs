//! This crate provides prefab support to Bevy through the usage of a plugin [`PrefabPlugin`] and two traits [`Prefab`]
//! and [`StaticPrefab`].
//!
//! Define either or both traits on a struct and register it with the respective function on the plugin.
//!
//! Note that registrations are bound by TypeId so if you do impl both traits on a struct, only the last will take
//! effect if [`PrefabPlugin::panic_on_double_registration`] is false, otherwise the application will panic as the name
//! indicates.
//!
//! Use the [`Prefabs`] resource to spawn a prefab of a type. Spawning unfortunately requires world access so it must be
//! ran on an exclusive system. A pattern to follow would be to setup everything you need in a normal system, and then
//! use [`Commands::queue`] with [`World::resource_scope`] to pull [`Prefabs`] from the world.
//!
//! See [`SpawnPrefabEvent`] for an example of this. This type can also be used if you do not care that an event is used
//! to spawn the prefab along with its untyped counterpart.

mod private;

use crate::private::{
    FolderLoadCheck, PrefabLoader,
    systems::{
        handle_descriptor_loads, handle_folder_loads, on_prefab_loaded, register_prefab,
        should_check_folder_loads, startup,
    },
    util::try_apply_bundle,
};
use bevy_app::prelude::*;
use bevy_asset::{LoadedFolder, prelude::*};
use bevy_ecs::{
    prelude::*,
    system::{SystemParam, SystemState},
};
use bevy_log::prelude::*;
use bevy_platform::collections::{HashMap, hash_map};
use bevy_reflect::{Reflect, Reflectable};
use bevy_utils::TypeIdMap;
use derive_more::{Deref, DerefMut};
use ordermap::OrderMap;
use serde::Deserialize;
use std::{
    any::TypeId,
    borrow::{Borrow, BorrowMut},
    marker::PhantomData,
    path::PathBuf,
};

/// A SystemParam QOL type to indicate no params are desired
#[derive(SystemParam)]
pub struct NoParams<'w, 's>(PhantomData<(&'w (), &'s ())>);

type RegistrationFn = Box<dyn Fn(&PrefabPlugin, &mut App) + Send + Sync>;

/// The base plugin for prefab support. Use the registration functions to add respective types.
pub struct PrefabPlugin {
    /// In the event a type is registered twice, panic
    panic_on_double_registration: bool,

    /// Add observers that you can trigger with [`SpawnPrefabEvent`] and [`SpawnUntypedPrefabEvent`]
    use_spawner_events: bool,

    registrations: OrderMap<TypeId, RegistrationFn>,
}

impl Default for PrefabPlugin {
    fn default() -> Self {
        Self {
            panic_on_double_registration: true,
            use_spawner_events: true,
            registrations: Default::default(),
        }
    }
}

impl PrefabPlugin {
    pub fn with_panic_on_double_registration(mut self, value: bool) -> Self {
        self.set_panic_on_double_registration(value);
        self
    }

    pub fn set_panic_on_double_registration(&mut self, value: bool) -> &mut Self {
        self.panic_on_double_registration = value;
        self
    }

    pub fn with_spawner_events(mut self, value: bool) -> Self {
        self.set_use_spawner_events(value);
        self
    }

    pub fn set_use_spawner_events(&mut self, value: bool) -> &mut Self {
        self.use_spawner_events = value;
        self
    }

    /// Register a prefab type using the singleton slot
    pub fn with_prefab<T>(mut self) -> Self
    where
        T: Prefab,
    {
        self.add_prefab::<T>();
        self
    }

    /// Register a prefab type using the singleton slot
    pub fn add_prefab<T>(&mut self) -> &mut Self
    where
        T: Prefab,
    {
        self.check_type_is_new::<T>();
        self.registrations.insert(
            TypeId::of::<T>(),
            Box::new(|this, app| {
                app.init_asset::<T::Descriptor>()
                    .register_asset_loader(PrefabLoader::<T>::default())
                    .add_observer(on_prefab_loaded::<T>.pipe(register_prefab::<T>))
                    .add_systems(Startup, startup::<T>)
                    .add_systems(FixedUpdate, handle_descriptor_loads::<T>);

                this.maybe_register_prefab_spawn_observer::<T>(app);

                if T::VARIANT_FIELD.is_some() {
                    app.insert_resource(FolderLoadCheck::<T>::new(true))
                        .add_systems(
                            FixedUpdate,
                            handle_folder_loads::<T>.run_if(should_check_folder_loads::<T>),
                        );
                }
            }),
        );
        self
    }

    /// Register a static prefab type using the singleton slot
    pub fn with_static_prefab<T>(mut self) -> Self
    where
        T: StaticPrefab,
    {
        self.add_static_prefab::<T>();
        self
    }

    /// Register a static prefab type using the singleton slot
    pub fn add_static_prefab<T>(&mut self) -> &mut Self
    where
        T: StaticPrefab,
    {
        self.check_type_is_new::<T>();
        self.registrations.insert(
            TypeId::of::<T>(),
            Box::new(|this, app| {
                this.maybe_register_prefab_spawn_observer::<T>(app);

                app.world_mut()
                    .resource_scope(|world, mut prefabs: Mut<Prefabs>| {
                        prefabs.register_static_prefab::<T>(world);
                    });
            }),
        );
        self
    }

    /// Register a named static prefab type
    pub fn with_static_prefab_variant<T>(mut self, name: impl Into<Name>) -> Self
    where
        T: StaticPrefab,
    {
        self.add_static_prefab_variant::<T>(name);
        self
    }

    /// Register a named static prefab type
    pub fn add_static_prefab_variant<T>(&mut self, name: impl Into<Name>) -> &mut Self
    where
        T: StaticPrefab,
    {
        self.check_type_is_new::<T>();
        let name = name.into();
        self.registrations.insert(
            TypeId::of::<T>(),
            Box::new(move |this, app| {
                this.maybe_register_prefab_spawn_observer::<T>(app);

                app.world_mut()
                    .resource_scope(|world, mut prefabs: Mut<Prefabs>| {
                        prefabs.register_static_prefab_variant::<T>(world, name.clone());
                    });
            }),
        );
        self
    }

    fn check_type_is_new<T>(&self)
    where
        T: 'static,
    {
        if self.registrations.contains_key(&TypeId::of::<T>()) {
            let name = std::any::type_name::<T>();
            if self.panic_on_double_registration {
                panic!("Tried to register {name} twice!");
            } else {
                warn!(
                    type_name = std::any::type_name::<T>(),
                    "Registered a type multiple times!"
                )
            }
        }
    }

    fn maybe_register_prefab_spawn_observer<T>(&self, app: &mut App)
    where
        T: Send + Sync + 'static,
    {
        if self.use_spawner_events {
            app.add_observer(SpawnPrefabEvent::<T>::handle);
        }
    }
}

impl Plugin for PrefabPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<Prefabs>();

        if self.use_spawner_events {
            app.add_observer(SpawnUntypedPrefabEvent::handle);
        }

        for f in self.registrations.values() {
            (f)(self, app);
        }
    }
}

/// The trait that marks a type as a valid Prefab
pub trait Prefab: Bundle + Sized {
    /// List of extensions to match for. Is used with
    /// [`AssetLoader::extensions`] for recursively loading an entire directory
    /// of prefabs.
    const EXTENSIONS: &[&str];

    /// If this is set the ron descriptor will first be deserialized into a
    /// value and this field will be used to get the property that represents
    /// the variant name.
    const VARIANT_FIELD: Option<&str> = None;

    /// A type that is deserialized from the source file and can be translated
    /// into the concrete prefab type.
    type Descriptor: Asset + Clone + Reflectable + for<'de> Deserialize<'de>;

    /// A SystemParam that will be passed into [`Prefab::spawn`] whenever it
    /// is called. Stores state so locals are also valid for use. This state is
    /// unique per type, not per variant.
    type Params<'w, 's>: for<'world, 'system> SystemParam<
        Item<'world, 'system> = Self::Params<'world, 'system>,
    >;

    /// Spawn an instance of this prefab.
    ///
    /// * `entity`: An empty entity spawned by the world, can be safely despawned if desired for any reason.
    fn spawn(entity: Entity, desc: Self::Descriptor, params: Self::Params<'_, '_>) -> Self;

    /// Returns the path to the directory that all prefabs should be loaded from.
    ///
    /// This recurses so in reality you could return the root of the assets if
    /// you wanted to load everything that matches the extensions.
    fn path() -> impl Into<PathBuf>;

    /// Deserialize the prefab into the descriptor type. By default prefabs use
    /// Ron, but any means of deserializing will do.
    fn deserialize(bytes: Vec<u8>) -> Result<Self::Descriptor> {
        Ok(ron::de::from_bytes(&bytes)?)
    }
}

/// Create a static prefab. This differs from [`Prefab`] such that the type
/// needs no descriptor nor is loaded from files. Instead the spawn logic is
/// programmatically controlled.
pub trait StaticPrefab: Bundle + Sized {
    type Params<'w, 's>: for<'world, 'system> SystemParam<
        Item<'world, 'system> = Self::Params<'world, 'system>,
    >;

    /// Spawn an instance of this prefab.
    ///
    /// * `entity`: An empty entity spawned by the world, can be safely despawned if desired for any reason.
    /// * `name`: The name of this variant, None if the default.
    fn spawn(entity: Entity, name: Option<Name>, params: Self::Params<'_, '_>) -> Self;
}

/// QOL trait if the prefab type is the same as its descriptor.
///
/// Forwards configuration to Prefab
pub trait IdentityPrefab: Asset + for<'de> Deserialize<'de> {
    const EXTENSIONS: &[&str];

    const VARIANT_FIELD: Option<&str>;

    fn path() -> impl Into<PathBuf>;

    fn deserialize(bytes: Vec<u8>) -> Result<Self> {
        Ok(ron::de::from_bytes(&bytes)?)
    }
}

impl<T> Prefab for T
where
    Self: IdentityPrefab + Reflectable + Bundle + Clone + Sized,
{
    const EXTENSIONS: &[&str] = <Self as IdentityPrefab>::EXTENSIONS;

    const VARIANT_FIELD: Option<&str> = <Self as IdentityPrefab>::VARIANT_FIELD;

    type Descriptor = Self;

    type Params<'w, 's> = NoParams<'w, 's>;

    fn spawn(_entity: Entity, desc: Self::Descriptor, _params: Self::Params<'_, '_>) -> Self {
        desc
    }

    fn path() -> impl Into<PathBuf> {
        <Self as IdentityPrefab>::path()
    }

    fn deserialize(bytes: Vec<u8>) -> Result<Self> {
        <Self as IdentityPrefab>::deserialize(bytes)
    }
}

type SpawnFn = Box<dyn (Fn(&mut World) -> Option<Entity>) + Send + Sync + 'static>;
type DescriptorFn = Box<dyn (Fn(&mut World) -> Option<Box<dyn Reflect>>) + Send + Sync + 'static>;

#[derive(Resource, Deref, DerefMut)]
struct StateHolder<T>(SystemState<T>)
where
    T: SystemParam + 'static;

type PrefabStateHolder<'w, 's, T> = StateHolder<<T as Prefab>::Params<'w, 's>>;
type StaticPrefabStateHolder<'w, 's, T> = StateHolder<<T as StaticPrefab>::Params<'w, 's>>;

pub struct PrefabMeta {
    handle: Option<UntypedHandle>,
    spawn_info: SpawnInfo,
}

pub enum SpawnInfo {
    Prefab {
        spawn_fn: SpawnFn,
        descriptor_fn: DescriptorFn,
    },
    StaticPrefab {
        spawn_fn: SpawnFn,
    },
}

impl SpawnInfo {
    pub fn spawn(&self, world: &mut World) -> Option<Entity> {
        match self {
            SpawnInfo::Prefab { spawn_fn, .. } => (spawn_fn)(world),
            SpawnInfo::StaticPrefab { spawn_fn } => (spawn_fn)(world),
        }
    }

    pub fn try_spawn_descriptor(&self, world: &mut World) -> Option<Box<dyn Reflect>> {
        match self {
            SpawnInfo::Prefab { descriptor_fn, .. } => (descriptor_fn)(world),
            SpawnInfo::StaticPrefab { .. } => None,
        }
    }
}

impl PrefabMeta {
    fn new(
        handle: UntypedHandle,
        spawn_fn: impl (Fn(&mut World) -> Option<Entity>) + Send + Sync + 'static,
        descriptor_fn: impl (Fn(&mut World) -> Option<Box<dyn Reflect>>) + Send + Sync + 'static,
    ) -> Self {
        Self {
            handle: Some(handle),
            spawn_info: SpawnInfo::Prefab {
                spawn_fn: Box::new(spawn_fn),
                descriptor_fn: Box::new(descriptor_fn),
            },
        }
    }

    fn new_static(
        spawn_fn: impl (Fn(&mut World) -> Option<Entity>) + Send + Sync + 'static,
    ) -> Self {
        Self {
            handle: None,
            spawn_info: SpawnInfo::StaticPrefab {
                spawn_fn: Box::new(spawn_fn),
            },
        }
    }

    pub fn handle(&self) -> Option<&UntypedHandle> {
        self.handle.as_ref()
    }

    fn spawn(&self, world: &mut World) -> Option<Entity> {
        self.spawn_info.spawn(world)
    }
}

/// A resource containing all known prefabs. Requires the world to spawn due to
/// usage of states.
#[derive(Resource, Default)]
pub struct Prefabs {
    prefabs: TypeIdMap<HashMap<Option<Name>, PrefabMeta>>,
}

impl Prefabs {
    /// Spawn a prefab with the supplied identifier.
    ///
    /// Returns the entity if it was successfully spawned
    pub fn spawn<T: 'static>(
        &self,
        world: &mut World,
        variant: impl Borrow<Option<Name>>,
    ) -> Option<Entity> {
        self.spawn_untyped(world, TypeId::of::<T>(), variant)
    }

    /// Spawn the type specified by the [`TypeId`] and its optional name
    ///
    /// Returns the entity if it was successfully spawned
    pub fn spawn_untyped(
        &self,
        world: &mut World,
        type_id: TypeId,
        variant: impl Borrow<Option<Name>>,
    ) -> Option<Entity> {
        let meta = self
            .prefabs
            .get(&type_id)
            .and_then(|variants| variants.get(variant.borrow()))?;

        meta.spawn(world)
    }

    /// Spawn the singleton prefab for a given type.
    ///
    /// Returns the entity if it was successfully spawned
    pub fn spawn_singleton<T>(&self, world: &mut World) -> Option<Entity>
    where
        T: 'static,
    {
        self.spawn::<T>(world, None)
    }

    /// Spawn the variant of a prefab for a given type.
    ///
    /// Returns the entity if it was successfully spawned
    pub fn spawn_variant<T>(&self, world: &mut World, name: impl Into<Name>) -> Option<Entity>
    where
        T: 'static,
    {
        self.spawn::<T>(world, Some(name.into()))
    }

    /// Return the number of prefabs registered.
    pub fn len(&self) -> usize {
        self.prefabs.len()
    }

    /// Return true if there are no prefabs registered.
    pub fn is_empty(&self) -> bool {
        self.prefabs.is_empty()
    }

    /// Return the number of prefab variants registered for a type. This includes the singleton if present.
    pub fn len_of_type<T>(&self) -> usize
    where
        T: 'static,
    {
        self.prefabs
            .get(&TypeId::of::<T>())
            .map(|m| m.len())
            .unwrap_or_default()
    }

    /// Check if the type has at least one registered prefab
    pub fn has_type<T>(&self) -> bool
    where
        T: 'static,
    {
        self.prefabs.contains_key(&TypeId::of::<T>())
    }

    /// Iterate all prefabs and their variants
    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (TypeId, hash_map::Iter<'_, Option<Name>, PrefabMeta>)> {
        self.prefabs
            .iter()
            .map(|(id, variants)| (*id, variants.iter()))
    }

    /// Iterate variants of a specific prefab
    pub fn iter_variants_of<T>(&self) -> impl Iterator<Item = &Option<Name>>
    where
        T: 'static,
    {
        self.iter_variants_of_untyped(TypeId::of::<T>())
    }

    /// Iterate variants of a specific prefab
    pub fn iter_variants_of_untyped(&self, type_id: TypeId) -> impl Iterator<Item = &Option<Name>> {
        self.prefabs
            .get(&type_id)
            .map(|variants| variants.keys())
            .unwrap_or_default()
    }

    /// Get the meta of a variant
    pub fn meta(&self, type_id: TypeId, variant: impl Borrow<Option<Name>>) -> Option<&PrefabMeta> {
        self.prefabs
            .get(&type_id)
            .and_then(|variants| variants.get(variant.borrow()))
    }

    /// Manually register a prefab using the supplied handle to the descriptor
    pub fn register_prefab<T: Prefab>(&mut self, handle: Handle<T::Descriptor>, world: &mut World) {
        self.internal_register_prefab::<T>(handle, world, None);
    }

    /// Manually register a prefab variant using the supplied handle to the descriptor
    pub fn register_prefab_variant<T: Prefab>(
        &mut self,
        handle: Handle<T::Descriptor>,
        world: &mut World,
        name: impl Into<Name>,
    ) {
        self.internal_register_prefab::<T>(handle, world, Some(name.into()));
    }

    /// Manually register a static prefab
    pub fn register_static_prefab<T: StaticPrefab>(&mut self, world: &mut World) {
        self.internal_register_static_prefab::<T>(world, None);
    }

    /// Manually register a static prefab variant
    pub fn register_static_prefab_variant<T: StaticPrefab>(
        &mut self,
        world: &mut World,
        name: impl Into<Name>,
    ) {
        self.internal_register_static_prefab::<T>(world, Some(name.into()));
    }

    fn internal_register_prefab<T: Prefab>(
        &mut self,
        handle: Handle<T::Descriptor>,
        world: &mut World,
        name: Option<Name>,
    ) {
        if !world.contains_resource::<PrefabStateHolder<T>>() {
            let state = SystemState::<T::Params<'_, '_>>::new(world);
            world.insert_resource(StateHolder(state));
        }

        let entry = self.prefabs.entry(TypeId::of::<T>()).or_default();
        let untyped_handle = handle.clone().untyped();
        let spawn_handle = handle.clone();
        let desc_handle = handle;

        entry.insert(
            name,
            PrefabMeta::new(
                untyped_handle,
                move |world: &mut World| {
                    let descriptors = world.resource::<Assets<T::Descriptor>>();
                    let Some(descriptor) = descriptors.get(spawn_handle.id()).cloned() else {
                        error!(
                            "Failed to get descriptor asset for {}",
                            std::any::type_name::<T>()
                        );
                        return None;
                    };

                    world.resource_scope(|world, mut state: Mut<PrefabStateHolder<T>>| {
                        let entity = world.spawn_empty().id();
                        let params = state.get_mut(world);
                        let bundle = T::spawn(entity, descriptor, params);
                        state.apply(world);

                        try_apply_bundle(world, entity, bundle)
                    })
                },
                move |world| {
                    let descriptors = world.resource::<Assets<T::Descriptor>>();
                    let Some(descriptor) = descriptors.get(desc_handle.id()).cloned() else {
                        error!(
                            "Failed to get descriptor asset for {}",
                            std::any::type_name::<T>()
                        );
                        return None;
                    };

                    Some(Box::new(descriptor))
                },
            ),
        );
    }

    fn internal_register_static_prefab<T: StaticPrefab>(
        &mut self,
        world: &mut World,
        name: Option<Name>,
    ) {
        if !world.contains_resource::<StaticPrefabStateHolder<T>>() {
            let state = SystemState::<T::Params<'_, '_>>::new(world);
            world.insert_resource(StateHolder(state));
        }

        let entry = self.prefabs.entry(TypeId::of::<T>()).or_default();
        entry.insert(
            name.clone(),
            PrefabMeta::new_static(move |world: &mut World| {
                world.resource_scope(|world, mut state: Mut<StaticPrefabStateHolder<T>>| {
                    let entity = world.spawn_empty().id();
                    let params = state.get_mut(world);
                    let bundle = T::spawn(entity, name.clone(), params);
                    state.apply(world);

                    try_apply_bundle(world, entity, bundle)
                })
            }),
        );
    }
}

/// Used as a means of keeping the prefab alive
/// If no longer needed this can be despawned
#[derive(Resource)]
pub struct SingletonPrefabHandle<T>(Handle<T::Descriptor>)
where
    T: Prefab;

impl<T> SingletonPrefabHandle<T>
where
    T: Prefab,
{
    pub fn handle(&self) -> &Handle<T::Descriptor> {
        &self.0
    }
}

/// A resource holding the handle to the folder the given prefab type was
/// loaded from. This can be removed after loading if reloads aren't desired
#[derive(Resource)]
pub struct PrefabFolderHandle<T>
where
    T: Prefab,
{
    handle: Handle<LoadedFolder>,
    _phantom_data: PhantomData<T>,
}

impl<T> PrefabFolderHandle<T>
where
    T: Prefab,
{
    pub fn new(handle: Handle<LoadedFolder>) -> Self {
        Self {
            handle,
            _phantom_data: Default::default(),
        }
    }
}

/// Triggered for every prefab of a given type that is loaded
#[derive(Event, Deref)]

pub struct PrefabLoadedEvent<T>(Handle<T::Descriptor>)
where
    T: Prefab;

/// Triggered when all the prefabs of the given type are loaded
#[derive(Event)]
pub struct PrefabsLoadedEvent<T>
where
    T: Prefab,
{
    handle: Handle<LoadedFolder>,
    _pd: PhantomData<T>,
}

impl<T> PrefabsLoadedEvent<T>
where
    T: Prefab,
{
    fn new(handle: Handle<LoadedFolder>) -> Self {
        Self {
            handle,
            _pd: Default::default(),
        }
    }

    pub fn handle(&self) -> &Handle<LoadedFolder> {
        &self.handle
    }
}

/// Triggered if the prefab type fails to load some prefabs
#[derive(Event)]
pub struct PrefabsLoadFailureEvent<T>
where
    T: Prefab,
{
    handle: Handle<LoadedFolder>,
    _pd: PhantomData<T>,
}

impl<T> PrefabsLoadFailureEvent<T>
where
    T: Prefab,
{
    fn new(handle: Handle<LoadedFolder>) -> Self {
        Self {
            handle,
            _pd: Default::default(),
        }
    }

    pub fn handle(&self) -> &Handle<LoadedFolder> {
        &self.handle
    }
}

/// An event used to spawn a prefab referenced by TypeId
#[derive(Event)]
pub struct SpawnUntypedPrefabEvent {
    type_id: TypeId,
    variant: Option<Name>,
}

impl SpawnUntypedPrefabEvent {
    pub fn new(type_id: TypeId, variant: impl Into<Option<Name>>) -> Self {
        Self {
            type_id,
            variant: variant.into(),
        }
    }

    fn handle(event: On<Self>, mut commands: Commands) {
        let type_id = event.type_id;
        let variant = event.variant.clone();
        commands.queue(move |world: &mut World| {
            world.resource_scope(|world, prefabs: Mut<Prefabs>| {
                if prefabs.spawn_untyped(world, type_id, &variant).is_none() {
                    world.trigger(UntypedPrefabSpawnFailureEvent::new(type_id, variant));
                }
            });
        });
    }
}

#[derive(Event)]
pub struct UntypedPrefabSpawnFailureEvent {
    type_id: TypeId,
    variant: Option<Name>,
}

impl UntypedPrefabSpawnFailureEvent {
    pub fn new(type_id: TypeId, variant: impl Into<Option<Name>>) -> Self {
        Self {
            type_id,
            variant: variant.into(),
        }
    }

    pub fn type_id(&self) -> TypeId {
        self.type_id
    }

    pub fn variant(&self) -> Option<&Name> {
        self.variant.as_ref()
    }
}

#[derive(Event)]
pub struct SpawnPrefabEvent<T> {
    variant: Option<Name>,
    _pd: PhantomData<T>,
}

impl<T> SpawnPrefabEvent<T>
where
    T: Send + Sync + 'static,
{
    pub fn new(variant: impl Into<Option<Name>>) -> Self {
        Self {
            variant: variant.into(),
            _pd: Default::default(),
        }
    }

    fn handle(event: On<Self>, mut commands: Commands) {
        let variant = event.variant.clone();
        commands.queue(move |world: &mut World| {
            world.resource_scope(|world, prefabs: Mut<Prefabs>| {
                if prefabs.spawn::<T>(world, &variant).is_none() {
                    world.trigger(PrefabSpawnFailureEvent::<T>::new(variant));
                }
            });
        });
    }
}

#[derive(Event)]
pub struct PrefabSpawnFailureEvent<T> {
    variant: Option<Name>,
    _pd: PhantomData<T>,
}

impl<T> PrefabSpawnFailureEvent<T> {
    pub fn new(variant: impl Into<Option<Name>>) -> Self {
        Self {
            variant: variant.into(),
            _pd: Default::default(),
        }
    }

    pub fn variant(&self) -> Option<&Name> {
        self.variant.as_ref()
    }
}

pub trait WorldExtensions: BorrowMut<World> {
    fn spawn_prefab<T>(&mut self, variant: Option<Name>) -> Option<Entity>
    where
        T: Prefab,
    {
        self.borrow_mut()
            .resource_scope(|world, prefabs: Mut<Prefabs>| prefabs.spawn::<T>(world, variant))
    }

    fn spawn_untyped_prefab(&mut self, type_id: TypeId, variant: Option<Name>) -> Option<Entity> {
        self.borrow_mut()
            .resource_scope(|world, prefabs: Mut<Prefabs>| {
                prefabs.spawn_untyped(world, type_id, variant)
            })
    }

    fn spawn_prefab_descriptor(
        &mut self,
        type_id: TypeId,
        variant: Option<Name>,
    ) -> Option<Box<dyn Reflect>> {
        self.borrow_mut()
            .resource_scope(|world, prefabs: Mut<Prefabs>| {
                prefabs
                    .meta(type_id, variant)
                    .and_then(|meta| meta.spawn_info.try_spawn_descriptor(world))
            })
    }
}

impl WorldExtensions for World {}

impl WorldExtensions for &mut World {}
