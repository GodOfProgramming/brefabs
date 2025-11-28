use crate::Prefab;
use bevy_asset::prelude::*;
use bevy_ecs::prelude::*;
use bevy_log::prelude::*;
use bevy_reflect::{PartialReflect, Reflect, ReflectKind};

pub enum PrefabRegistrationResult {
    FoundKey(Option<String>),
    Fail,
}

pub fn get_variant_name<T>(
    world: &mut World,
    handle: &Handle<T::Descriptor>,
    key: &str,
) -> PrefabRegistrationResult
where
    T: Prefab,
{
    world.resource_scope(|_, descriptors: Mut<Assets<T::Descriptor>>| {
        let Some(descriptor) = descriptors.get(handle) else {
            warn!(
                type_name = std::any::type_name::<T>(),
                "Failed to get prefab descriptor",
            );
            return PrefabRegistrationResult::Fail;
        };

        let reflected = descriptor.as_reflect();

        match reflected.reflect_kind() {
            ReflectKind::Struct => get_field_key_from_struct(reflected, key),
            ReflectKind::TupleStruct => {
                let Ok(key) = key.parse() else {
                    error!(key, "Variant key is not valid for tuple struct");
                    return PrefabRegistrationResult::Fail;
                };

                get_field_key_from_tuple_struct(reflected, key)
            }
            _ => None,
        }
        .and_then(|field: &dyn PartialReflect| {
            field
                .try_downcast_ref::<String>()
                .cloned()
                .map(Some)
                .or_else(|| {
                    field
                        .try_downcast_ref::<Option<String>>()
                        .map(|opt| opt.as_ref().cloned())
                })
        })
        .map(PrefabRegistrationResult::FoundKey)
        .unwrap_or(PrefabRegistrationResult::Fail)
    })
}

fn get_field_key_from_struct<'a>(
    reflected: &'a dyn Reflect,
    key: &str,
) -> Option<&'a dyn PartialReflect> {
    let reflected = reflected
        .reflect_ref()
        .as_struct()
        .expect("Should be a struct");

    Some(reflected.field(key)).flatten()
}

fn get_field_key_from_tuple_struct(
    reflected: &dyn Reflect,
    key: usize,
) -> Option<&dyn PartialReflect> {
    let reflected = reflected
        .reflect_ref()
        .as_tuple_struct()
        .expect("Should be a tuple struct");

    Some(reflected.field(key)).flatten()
}

pub fn try_apply_bundle(world: &mut World, entity: Entity, bundle: impl Bundle) -> Option<Entity> {
    if let Ok(mut entity_ref) = world.get_entity_mut(entity) {
        entity_ref.insert(bundle);
        Some(entity)
    } else {
        None
    }
}
