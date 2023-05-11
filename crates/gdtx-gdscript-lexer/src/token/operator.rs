use serde::{Deserialize, Serialize};

/// Operator.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Copy, Clone, Serialize, Deserialize)]
pub enum Operator {
    AddAssign,
    AndAssign,
    BinAnd,
    BinEq,
    BinNotEq,
    BinOr,
    BitLShift,
    BitRShift,
    DivAssign,
    GreaterOrEq,
    Infer,
    LowerOrEq,
    ModAssign,
    MulAssign,
    OrAssign,
    SubAssign,
    TwoDots,
    TypeArrow,
    XorAssign,
}
