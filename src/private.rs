pub mod systems;
pub mod util;

use crate::Prefab;
use bevy_asset::{AssetLoader, LoadContext, io::Reader};
use bevy_ecs::prelude::*;
use derive_more::{Deref, DerefMut};
use std::marker::PhantomData;

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
        let descriptor: <T as Prefab>::Descriptor = T::deserialize(bytes)?;
        Ok(descriptor)
    }

    fn extensions(&self) -> &[&str] {
        T::EXTENSIONS
    }
}

#[derive(Resource, Deref, DerefMut)]
pub struct FolderLoadCheck<T>
where
    T: Prefab,
{
    #[deref]
    #[deref_mut]
    state: bool,
    _pd: PhantomData<T>,
}

impl<T> FolderLoadCheck<T>
where
    T: Prefab,
{
    pub fn new(check: bool) -> Self {
        Self {
            state: check,
            _pd: Default::default(),
        }
    }
}
