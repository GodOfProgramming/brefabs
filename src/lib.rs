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

mod private;

use crate::private::{
    FolderLoadCheck, PrefabLoader,
    systems::{
        handle_descriptor_loads, handle_folder_loads, on_prefab_loaded, register_prefab,
        should_check_folder_loads, startup,
    },
};
use bevy_app::prelude::*;
use bevy_asset::{LoadedFolder, prelude::*};
use bevy_ecs::{
    prelude::*,
    system::{SystemParam, SystemState},
};
use bevy_log::prelude::*;
use bevy_platform::collections::HashMap;
use bevy_reflect::{Reflectable, TypeRegistration, TypeRegistry};
use bevy_utils::TypeIdMap;
use derive_more::{Deref, DerefMut};
use ordermap::OrderMap;
use serde::Deserialize;
use std::{any::TypeId, borrow::Borrow, marker::PhantomData, path::PathBuf};

/// A SystemParam QOL type to indicate no params are desired
#[derive(SystemParam)]
pub struct NoParams<'w, 's>(PhantomData<(&'w (), &'s ())>);

type RegistrationFn = Box<dyn Fn(&mut App) + Send + Sync>;

/// The base plugin for prefab support. Use the registration functions to add respective types.
#[derive(Default)]
pub struct PrefabPlugin {
    registrations: OrderMap<TypeId, RegistrationFn>,
    panic_on_double_registration: bool,
}

impl PrefabPlugin {
    /// Register a prefab type using the default slot
    pub fn with_prefab<T>(mut self) -> Self
    where
        T: Prefab,
    {
        self.add_prefab::<T>();
        self
    }

    /// Register a static prefab type using the default slot
    pub fn with_static_prefab<T>(mut self) -> Self
    where
        T: StaticPrefab,
    {
        self.add_static_prefab::<T>();
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

    /// Register a prefab type using the default slot
    pub fn add_prefab<T>(&mut self) -> &mut Self
    where
        T: Prefab,
    {
        self.check_type_is_new::<T>();
        self.registrations.insert(
            TypeId::of::<T>(),
            Box::new(|app| {
                app.init_asset::<T::Descriptor>()
                    .register_asset_loader(PrefabLoader::<T>::default())
                    .add_observer(on_prefab_loaded::<T>.pipe(register_prefab::<T>))
                    .add_systems(Startup, startup::<T>)
                    .add_systems(FixedUpdate, handle_descriptor_loads::<T>);

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

    /// Register a static prefab type using the default slot
    pub fn add_static_prefab<T>(&mut self) -> &mut Self
    where
        T: StaticPrefab,
    {
        self.check_type_is_new::<T>();
        self.registrations.insert(
            TypeId::of::<T>(),
            Box::new(|app| {
                app.world_mut()
                    .resource_scope(|world, mut prefabs: Mut<Prefabs>| {
                        prefabs.register_static_prefab::<T>(world);
                    });
            }),
        );
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
            Box::new(move |app| {
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
}

impl Plugin for PrefabPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<Prefabs>();

        for f in self.registrations.values() {
            (f)(app);
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

type SpawnFn = Box<dyn FnMut(&mut World) + Send + Sync>;

#[derive(Resource, Deref, DerefMut)]
struct StateHolder<T>(SystemState<T>)
where
    T: SystemParam + 'static;

type PrefabStateHolder<'w, 's, T> = StateHolder<<T as Prefab>::Params<'w, 's>>;

/// A resource containing all known prefabs. Requires the world to spawn due to
/// usage of states.
#[derive(Resource, Default)]
pub struct Prefabs {
    prefabs: TypeIdMap<HashMap<Option<Name>, SpawnFn>>,
}

impl Prefabs {
    /// Spawn a prefab with the supplied identifier.
    pub fn spawn<T: 'static>(&mut self, world: &mut World, key: impl Borrow<Option<Name>>) -> bool {
        let Some(spawner) = self
            .prefabs
            .get_mut(&TypeId::of::<T>())
            .and_then(|variants| variants.get_mut(key.borrow()))
        else {
            return false;
        };

        (spawner)(world);

        true
    }

    /// Spawn the nameless prefab for a given type.
    ///
    /// Returns true if successful.
    pub fn spawn_nameless<T>(&mut self, world: &mut World) -> bool
    where
        T: 'static,
    {
        self.spawn::<T>(world, None)
    }

    /// Spawn the variant of a prefab for a given type.
    ///
    /// Returns true if successful.
    pub fn spawn_variant<T>(&mut self, world: &mut World, name: impl Into<Name>) -> bool
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

    /// Return the number of prefab variants registered for a type. This includes the nameless if present.
    pub fn len_of_type<T>(&self) -> usize
    where
        T: 'static,
    {
        self.prefabs
            .get(&TypeId::of::<T>())
            .map(|m| m.len())
            .unwrap_or_default()
    }

    pub fn has_type<T>(&self) -> bool
    where
        T: 'static,
    {
        self.prefabs.contains_key(&TypeId::of::<T>())
    }

    pub fn iter(&self) -> impl Iterator {
        self.prefabs.iter()
    }

    pub fn iter_type<T>(&self) -> impl Iterator<Item = (&Option<Name>, &SpawnFn)>
    where
        T: 'static,
    {
        self.prefabs
            .get(&TypeId::of::<T>())
            .map(|variants| variants.iter())
            .unwrap_or_default()
    }

    pub fn iter_variants<T>(&self) -> impl Iterator<Item = &Option<Name>>
    where
        T: 'static,
    {
        self.prefabs
            .get(&TypeId::of::<T>())
            .map(|variants| variants.keys())
            .unwrap_or_default()
    }

    pub fn iter_all_types<'t>(
        &self,
        type_registry: &'t TypeRegistry,
    ) -> impl Iterator<Item = (&'t TypeRegistration, &HashMap<Option<Name>, SpawnFn>)> {
        self.prefabs
            .iter()
            .filter_map(|(id, variants)| type_registry.get(*id).map(|t| (t, variants)))
    }

    pub fn register_prefab<T: Prefab>(&mut self, handle: Handle<T::Descriptor>, world: &mut World) {
        self.internal_register_prefab::<T>(handle, world, None);
    }

    pub fn register_prefab_variant<T: Prefab>(
        &mut self,
        handle: Handle<T::Descriptor>,
        world: &mut World,
        name: impl Into<Name>,
    ) {
        self.internal_register_prefab::<T>(handle, world, Some(name.into()));
    }

    pub fn register_static_prefab<T: StaticPrefab>(&mut self, world: &mut World) {
        self.internal_register_static_prefab::<T>(world, None);
    }

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
        entry.insert(
            name,
            Box::new(move |world: &mut World| {
                let descriptors = world.resource::<Assets<T::Descriptor>>();
                let Some(descriptor) = descriptors.get(handle.id()).cloned() else {
                    error!(
                        "Failed to get descriptor asset for {}",
                        std::any::type_name::<T>()
                    );
                    return;
                };

                let entity = world.spawn_empty().id();

                world.resource_scope(|world, mut state: Mut<PrefabStateHolder<T>>| {
                    let params = state.get_mut(world);
                    let bundle = T::spawn(entity, descriptor, params);
                    state.apply(world);

                    if let Ok(mut entity) = world.get_entity_mut(entity) {
                        entity.insert(bundle);
                    }
                });
            }),
        );
    }

    fn internal_register_static_prefab<T: StaticPrefab>(
        &mut self,
        world: &mut World,
        name: Option<Name>,
    ) {
        let mut state = SystemState::<T::Params<'_, '_>>::new(world);
        let entry = self.prefabs.entry(TypeId::of::<T>()).or_default();
        entry.insert(
            name.clone(),
            Box::new(move |world: &mut World| {
                let entity = world.spawn_empty().id();
                let params = state.get_mut(world);
                let bundle = T::spawn(entity, name.clone(), params);
                if let Ok(mut entity) = world.get_entity_mut(entity) {
                    entity.insert(bundle);
                }
            }),
        );
    }
}

#[derive(SystemParam, Deref)]
pub struct PrefabDescriptors<'w, T>
where
    T: Prefab,
{
    descriptors: Res<'w, Assets<<T as Prefab>::Descriptor>>,
}

#[derive(Resource)]
pub struct SinglePrefabHandle<T>(Handle<T::Descriptor>)
where
    T: Prefab;

impl<T> SinglePrefabHandle<T>
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
