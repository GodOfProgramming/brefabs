use std::time::Duration;

use bevy::{prelude::*, time::common_conditions::on_timer};
use bevy_ecs::system::SystemParam;
use brefabs::{Prefab, PrefabPlugin};
use serde::Deserialize;

fn main() {
    App::new()
        .init_resource::<PrefabsLoaded>()
        .add_plugins((
            DefaultPlugins,
            PrefabPlugin::default()
                .with_prefab::<ExamplePrefab>()
                .with_prefab::<Singleton>(),
        ))
        .add_observer(on_all_loaded)
        .add_observer(on_folder_failure)
        .add_systems(Startup, startup)
        .add_systems(
            FixedUpdate,
            spawn_cubes
                .run_if(have_less_than::<10, Mesh3d>)
                .run_if(have_prefabs)
                .run_if(on_timer(Duration::from_secs(1))),
        )
        .run();
}

#[derive(Bundle)]
struct ExamplePrefab {
    name: Name,
    mesh: Mesh3d,
    mesh_material: MeshMaterial3d<StandardMaterial>,
    transform: Transform,
}

#[derive(Asset, Reflect, Clone, Deserialize)]
struct ExamplePrefabDescriptor {
    variant: String,

    color: Color,
}

struct Spiral {
    theta: f32,
    r: f32,
    h: f32,
}

impl Default for Spiral {
    fn default() -> Self {
        Self {
            theta: 0.0,
            r: 2.0,
            h: 0.0,
        }
    }
}

#[derive(SystemParam)]
struct Params<'w, 's> {
    meshes: ResMut<'w, Assets<Mesh>>,
    materials: ResMut<'w, Assets<StandardMaterial>>,
    spiral: Local<'s, Spiral>,
}

impl Prefab for ExamplePrefab {
    const EXTENSIONS: &[&str] = &["ron"];

    const VARIANT_FIELD: Option<&str> = Some("variant");

    type Descriptor = ExamplePrefabDescriptor;

    type Params<'w, 's> = Params<'w, 's>;

    fn spawn(_entity: Entity, desc: Self::Descriptor, mut params: Self::Params<'_, '_>) -> Self {
        let offset = Vec2::new(
            params.spiral.r * params.spiral.theta.cos(),
            params.spiral.r * params.spiral.theta.sin(),
        );

        params.spiral.theta += 30.0f32.to_radians();
        params.spiral.h += 0.5;

        Self {
            name: Name::new(desc.variant),
            mesh: Mesh3d(params.meshes.add(Cuboid::new(1.0, 1.0, 1.0))),
            mesh_material: MeshMaterial3d(params.materials.add(Color::srgb_u8(124, 144, 255))),
            transform: Transform::from_xyz(offset.x, params.spiral.h, offset.y),
        }
    }

    fn path() -> impl Into<std::path::PathBuf> {
        "prefabs/many"
    }
}

#[derive(Asset, Bundle, Reflect, Clone, Deserialize)]
struct Singleton {
    name: Name,
}

impl brefabs::IdentityPrefab for Singleton {
    const EXTENSIONS: &[&str] = &[];

    const VARIANT_FIELD: Option<&str> = None;

    fn path() -> impl Into<std::path::PathBuf> {
        "prefabs/singleton.ron"
    }
}

#[derive(Resource, Default)]
struct PrefabsLoaded(bool);

fn startup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // circular base
    commands.spawn((
        Name::new("Base"),
        Mesh3d(meshes.add(Circle::new(4.0))),
        MeshMaterial3d(materials.add(Color::WHITE)),
        Transform::from_rotation(Quat::from_rotation_x(-std::f32::consts::FRAC_PI_2)),
    ));
    // cube
    commands.spawn((
        Name::new("Cube"),
        Mesh3d(meshes.add(Cuboid::new(1.0, 1.0, 1.0))),
        MeshMaterial3d(materials.add(Color::srgb_u8(124, 144, 255))),
        Transform::from_xyz(0.0, 0.5, 0.0),
    ));
    // light
    commands.spawn((
        Name::new("Light"),
        PointLight {
            shadows_enabled: true,
            ..default()
        },
        Transform::from_xyz(4.0, 8.0, 4.0),
    ));
    // camera
    commands.spawn((
        Name::new("Game Camera"),
        Camera3d::default(),
        Transform::from_xyz(-2.5, 4.5, 9.0).looking_at(Vec3::ZERO, Vec3::Y),
    ));
}

fn on_all_loaded(_: On<brefabs::PrefabsLoadedEvent<ExamplePrefab>>, mut commands: Commands) {
    info!("all prefabs loaded");
    commands.insert_resource(PrefabsLoaded(true));
}

fn on_folder_failure(_: On<brefabs::PrefabsLoadFailureEvent<ExamplePrefab>>) {
    error!("example failed to load");
}

fn spawn_cubes(world: &mut World) {
    world.resource_scope(|world, mut prefabs: Mut<brefabs::Prefabs>| {
        if !prefabs.spawn_variant::<ExamplePrefab>(world, "red") {
            error!("failed to spawn variant")
        }
    });
}

fn have_less_than<const N: usize, C: Component>(q_components: Query<&C>) -> bool {
    q_components.iter().len() < N
}

fn have_prefabs(state: Res<PrefabsLoaded>) -> bool {
    state.0
}
