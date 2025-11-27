use bevy_app::prelude::*;
use bevy_asset::{AssetLoader, LoadContext, LoadedFolder, io::Reader, prelude::*};
use bevy_ecs::{
  prelude::*,
  system::{SystemParam, SystemState},
};
use bevy_log::prelude::*;
use bevy_platform::{collections::HashMap, hash::NoOpHash};
use bevy_reflect::{PartialReflect, Reflect, ReflectKind, Reflectable, TypeRegistry};
use bevy_utils::TypeIdMap;
use derive_more::Deref;
use serde::Deserialize;
use std::{any::TypeId, marker::PhantomData, path::PathBuf};

#[derive(SystemParam)]
pub struct NoParams<'w, 's>(PhantomData<(&'w (), &'s ())>);

#[derive(Default)]
pub struct PrefabPlugin {
  prefab_registrations: Vec<Box<dyn Fn(&mut App) + Send + Sync>>,
  static_prefab_registrations: Vec<Box<dyn Fn(&mut World, &mut Prefabs) + Send + Sync>>,
}

impl PrefabPlugin {
  pub fn with_prefab<T>(mut self) -> Self
  where
    T: Prefab,
  {
    self.add_prefab::<T>();
    self
  }

  pub fn with_static_prefab<T>(mut self) -> Self
  where
    T: StaticPrefab,
  {
    self.add_static_prefab::<T>();
    self
  }

  pub fn with_static_prefab_variant<T>(mut self, name: impl Into<Name>) -> Self
  where
    T: StaticPrefab,
  {
    self.add_static_prefab_variant::<T>(name);
    self
  }

  pub fn add_prefab<T>(&mut self) -> &mut Self
  where
    T: Prefab,
  {
    self.prefab_registrations.push(Box::new(|app| {
      app
        .init_asset::<T::Descriptor>()
        .register_asset_loader(PrefabLoader::<T>::default())
        .add_observer(on_prefab_loaded::<T>.pipe(register_prefab::<T>))
        // on startup create a prefab loader
        .add_systems(Startup, startup::<T>)
        // then read all events that come in for the loaded prefab
        .add_systems(FixedUpdate, on_folder_notify::<T>);
    }));
    self
  }

  pub fn add_static_prefab<T>(&mut self) -> &mut Self
  where
    T: StaticPrefab,
  {
    self
      .static_prefab_registrations
      .push(Box::new(|world, prefabs| {
        prefabs.register_static_prefab::<T>(world);
      }));
    self
  }

  pub fn add_static_prefab_variant<T>(&mut self, name: impl Into<Name>) -> &mut Self
  where
    T: StaticPrefab,
  {
    let name = name.into();
    self
      .static_prefab_registrations
      .push(Box::new(move |world, prefabs| {
        prefabs.register_static_prefab_variant::<T>(world, name.clone());
      }));
    self
  }
}

impl Plugin for PrefabPlugin {
  fn build(&self, app: &mut App) {
    let mut prefabs = Prefabs::default();

    for f in &self.static_prefab_registrations {
      (f)(app.world_mut(), &mut prefabs);
    }

    app.insert_resource::<Prefabs>(prefabs);

    for f in &self.prefab_registrations {
      (f)(app);
    }
  }
}

pub trait Prefab: Bundle + Sized {
  const EXTENSIONS: &[&str];

  /// If this is set the ron descriptor will first be deserialized
  /// into a value and this name will be used to get the property
  const VARIANT_KEY: Option<&str> = None;

  type Descriptor: Asset + Reflectable + Clone + for<'de> Deserialize<'de>;

  type Params<'w, 's>: for<'world, 'system> SystemParam<
    Item<'world, 'system> = Self::Params<'world, 'system>,
  >;

  fn spawn(entity: Entity, desc: Self::Descriptor, params: Self::Params<'_, '_>) -> Self;

  fn path() -> impl Into<PathBuf>;
}

pub trait StaticPrefab: Bundle + Sized {
  type Params<'w, 's>: for<'world, 'system> SystemParam<
    Item<'world, 'system> = Self::Params<'world, 'system>,
  >;

  fn spawn(entity: Entity, params: Self::Params<'_, '_>) -> Self;
}

pub trait IdentityPrefab: Asset + Clone + for<'de> Deserialize<'de> {
  const EXTENSIONS: &[&str];

  fn path() -> impl Into<PathBuf>;
}

impl<T> Prefab for T
where
  T: IdentityPrefab + Reflectable + Bundle + Sized,
{
  const EXTENSIONS: &[&str] = <Self as IdentityPrefab>::EXTENSIONS;

  type Descriptor = Self;

  type Params<'w, 's> = NoParams<'w, 's>;

  fn spawn(_entity: Entity, desc: Self::Descriptor, _params: Self::Params<'_, '_>) -> Self {
    desc
  }

  fn path() -> impl Into<PathBuf> {
    <Self as IdentityPrefab>::path()
  }
}

type SpawnFn = Box<dyn FnMut(&mut World) + Send + Sync>;

#[derive(Resource, Default)]
pub struct Prefabs {
  prefabs: TypeIdMap<HashMap<Option<Name>, SpawnFn, NoOpHash>>,
}

impl Prefabs {
  pub fn spawn<T>(&mut self, world: &mut World) -> bool
  where
    T: 'static,
  {
    self.internal_spawn::<T>(world, &None)
  }

  pub fn spawn_variant<T>(&mut self, world: &mut World, name: impl Into<Name>) -> bool
  where
    T: 'static,
  {
    self.internal_spawn::<T>(world, &Some(name.into()))
  }

  pub fn iter(&self) -> impl Iterator {
    self.prefabs.iter()
  }

  pub fn iter_types(&self, type_registry: &TypeRegistry) -> impl Iterator {
    self
      .prefabs
      .iter()
      .filter_map(|(id, variants)| type_registry.get(*id).and_then(|t| Some((t, variants))))
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

  fn internal_spawn<T: 'static>(&mut self, world: &mut World, name: &Option<Name>) -> bool {
    let Some(spawner) = self
      .prefabs
      .get_mut(&TypeId::of::<T>())
      .and_then(|variants| variants.get_mut(name))
    else {
      return false;
    };

    (spawner)(world);

    true
  }

  fn internal_register_prefab<T: Prefab>(
    &mut self,
    handle: Handle<T::Descriptor>,
    world: &mut World,
    name: Option<Name>,
  ) {
    let mut state = SystemState::<T::Params<'_, '_>>::new(world);
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
        let params = state.get_mut(world);
        let bundle = T::spawn(entity, descriptor, params);
        world.entity_mut(entity).insert(bundle);
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
      name,
      Box::new(move |world: &mut World| {
        let entity = world.spawn_empty().id();
        let params = state.get_mut(world);
        let bundle = T::spawn(entity, params);
        world.entity_mut(entity).insert(bundle);
      }),
    );
  }
}

fn startup<T>(mut commands: Commands, assets: ResMut<AssetServer>)
where
  T: Prefab,
{
  let handle = assets.load_folder(T::path().into());
  commands.insert_resource(PrefabFolderHandle::<T>::new(handle));
}

fn on_folder_notify<T>(
  mut commands: Commands,
  mut event_reader: MessageReader<AssetEvent<LoadedFolder>>,
  loaded_folders: Res<Assets<LoadedFolder>>,
  folder: Res<PrefabFolderHandle<T>>,
) where
  T: Prefab,
{
  let handles = event_reader
    .read()
    .filter_map(|event| {
      if event.is_loaded_with_dependencies(folder.handle()) {
        loaded_folders.get(folder.handle())
      } else {
        None
      }
    })
    .map(|folder| folder.handles.iter().cloned())
    .flatten();

  for handle in handles {
    let typed_handle = handle.typed_debug_checked::<T::Descriptor>();
    commands.trigger(PrefabLoadedEvent::<T>(typed_handle));
  }
}

fn on_prefab_loaded<T>(event: On<PrefabLoadedEvent<T>>) -> Handle<T::Descriptor>
where
  T: Prefab,
{
  event.0.clone()
}

fn register_prefab<T>(handle: In<Handle<T::Descriptor>>, world: &mut World)
where
  T: Prefab,
{
  world.resource_scope(|world, mut prefabs: Mut<Prefabs>| match T::VARIANT_KEY {
    Some(key) => {
      let variant = world.resource_scope(|_world, descriptors: Mut<Assets<T::Descriptor>>| {
        let Some(descriptor) = descriptors.get(handle.id()).cloned() else {
          warn!(
            "Failed to get prefab descriptor for {}",
            std::any::type_name::<T>()
          );
          return None;
        };

        let reflected = descriptor.as_reflect();

        'reflect_test: {
          match reflected.reflect_kind() {
            ReflectKind::Struct => {
              let reflected = reflected
                .reflect_ref()
                .as_struct()
                .expect("Should be a struct");

              Some(reflected.field(key)).flatten()
            }
            ReflectKind::TupleStruct => {
              let Ok(key) = key.parse() else {
                break 'reflect_test None;
              };

              let reflected = reflected
                .reflect_ref()
                .as_tuple_struct()
                .expect("Should be a tuple struct");

              Some(reflected.field(key)).flatten()
            }
            _ => None,
          }
        }
        .and_then(|field: &dyn PartialReflect| {
          field.try_downcast_ref::<String>().cloned().or_else(|| {
            field
              .try_downcast_ref::<Option<String>>()
              .cloned()
              .flatten()
          })
        })
      });

      if let Some(variant) = variant {
        prefabs.register_prefab_variant::<T>(handle.clone(), world, variant);
      }
    }
    None => {
      prefabs.register_prefab::<T>(handle.clone(), world);
    }
  });
}

pub struct PrefabLoader<T>
where
  T: Prefab,
{
  _phantom_data: PhantomData<T>,
}

impl<T> Default for PrefabLoader<T>
where
  T: Prefab,
{
  fn default() -> Self {
    Self {
      _phantom_data: Default::default(),
    }
  }
}

impl<T> AssetLoader for PrefabLoader<T>
where
  T: Prefab,
{
  type Asset = <T as Prefab>::Descriptor;

  type Settings = ();

  type Error = BevyError;

  async fn load(
    &self,
    reader: &mut dyn Reader,
    _settings: &Self::Settings,
    _load_context: &mut LoadContext<'_>,
  ) -> Result<Self::Asset, Self::Error> {
    let mut bytes = Vec::new();
    reader.read_to_end(&mut bytes).await?;
    let ron: <T as Prefab>::Descriptor = ron::de::from_bytes(&bytes)?;
    Ok(ron)
  }

  fn extensions(&self) -> &[&str] {
    T::EXTENSIONS
  }
}

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

  pub fn handle(&self) -> &Handle<LoadedFolder> {
    &self.handle
  }
}

#[derive(Event, Deref)]

pub struct PrefabLoadedEvent<T>(Handle<T::Descriptor>)
where
  T: Prefab;
