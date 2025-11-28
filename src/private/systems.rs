use bevy_asset::{RecursiveDependencyLoadState, prelude::*};
use bevy_ecs::prelude::*;
use bevy_log::prelude::*;

use crate::{
    Prefab, PrefabFolderHandle, PrefabLoadedEvent, Prefabs, PrefabsLoadFailureEvent,
    PrefabsLoadedEvent, SingletonPrefabHandle,
    private::{
        FolderLoadCheck,
        util::{PrefabRegistrationResult, get_variant_name},
    },
};

pub fn startup<T>(mut commands: Commands, assets: ResMut<AssetServer>)
where
    T: Prefab,
{
    if T::VARIANT_FIELD.is_some() {
        let handle = assets.load_folder(T::path().into());
        commands.insert_resource(PrefabFolderHandle::<T>::new(handle));
    } else {
        let handle = assets.load(T::path().into());
        commands.insert_resource(SingletonPrefabHandle::<T>(handle));
    }
}

pub fn handle_descriptor_loads<T>(
    mut commands: Commands,
    mut messages: MessageReader<AssetEvent<T::Descriptor>>,
    mut assets: ResMut<Assets<T::Descriptor>>,
) where
    T: Prefab,
{
    for msg in messages.read() {
        if let AssetEvent::LoadedWithDependencies { id } = msg {
            let Some(handle) = assets.get_strong_handle(*id) else {
                unreachable!();
            };

            commands.trigger(PrefabLoadedEvent::<T>(handle));
        }
    }
}

pub fn should_check_folder_loads<T>(
    folder_handle: Option<Res<PrefabFolderHandle<T>>>,
    should_check: Res<FolderLoadCheck<T>>,
) -> bool
where
    T: Prefab,
{
    folder_handle.is_some() && **should_check
}

pub fn handle_folder_loads<T>(
    mut commands: Commands,
    assets: Res<AssetServer>,
    folder: Res<PrefabFolderHandle<T>>,
    mut should_check: ResMut<FolderLoadCheck<T>>,
) where
    T: Prefab,
{
    let Some(state) = assets.get_recursive_dependency_load_state(&folder.handle) else {
        return;
    };

    match state {
        RecursiveDependencyLoadState::Loaded => {
            **should_check = false;
            commands.trigger(PrefabsLoadedEvent::<T>::new(folder.handle.clone()));
        }
        RecursiveDependencyLoadState::Failed(asset_load_error) => {
            error!(
                type_name = std::any::type_name::<T>(),
                err = asset_load_error.to_string(),
                "Failed to load prefab"
            );
            **should_check = false;
            commands.trigger(PrefabsLoadFailureEvent::<T>::new(folder.handle.clone()));
        }
        _ => {
            // do nothing
        }
    }
}

pub fn on_prefab_loaded<T>(event: On<PrefabLoadedEvent<T>>) -> Handle<T::Descriptor>
where
    T: Prefab,
{
    event.0.clone()
}

pub fn register_prefab<T>(handle: In<Handle<T::Descriptor>>, mut commands: Commands)
where
    T: Prefab,
{
    let handle = handle.clone();
    commands.queue(move |world: &mut World| {
        world.resource_scope(|world, mut prefabs: Mut<Prefabs>| {
            let found_key: Option<PrefabRegistrationResult> =
                T::VARIANT_FIELD.map(|key| get_variant_name::<T>(world, &handle, key));

            match found_key {
                Some(result) => match result {
                    PrefabRegistrationResult::FoundKey(found_key) => match found_key {
                        Some(variant) => {
                            prefabs.register_prefab_variant::<T>(
                                handle.clone(),
                                world,
                                Name::from(variant),
                            );
                        }
                        None => {
                            prefabs.register_prefab::<T>(handle.clone(), world);
                        }
                    },
                    PrefabRegistrationResult::Fail => {
                        warn!(
                            type_name = std::any::type_name::<T>(),
                            "Failed to register prefab variant",
                        );
                    }
                },
                None => {
                    prefabs.register_prefab::<T>(handle.clone(), world);
                }
            }
        });
    });
}
