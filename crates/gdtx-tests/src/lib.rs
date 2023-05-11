//! Test utilities for gdtx

#![deny(missing_docs)]

/// Setup test context.
///
/// - Activate a preconfigured tracing subscriber
pub fn setup() {
    use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};
    use tracing_tree::HierarchicalLayer;

    let env_layer = EnvFilter::from_default_env();
    let subscriber = Registry::default().with(env_layer).with(
        HierarchicalLayer::new(2)
            .with_bracketed_fields(true)
            .with_targets(true),
    );

    tracing::subscriber::set_global_default(subscriber).ok();
}
