use std::collections::HashMap;

pub type IntType = i64;
pub type FloatType = f64;

#[derive(Debug)]
pub struct GdIdentifier(String);
#[derive(Debug)]
pub struct GdClassName(String);
#[derive(Debug)]
pub struct GdTypeName(String);

impl From<&'static str> for GdIdentifier {
    fn from(value: &'static str) -> Self {
        Self(value.into())
    }
}

impl From<&'static str> for GdClassName {
    fn from(value: &'static str) -> Self {
        Self(value.into())
    }
}

impl From<&'static str> for GdTypeName {
    fn from(value: &'static str) -> Self {
        Self(value.into())
    }
}

#[derive(Debug)]
pub struct GdClass {
    name: GdIdentifier,
    inherits_from: GdClassName,
    constants: Vec<GdConstant>,
    members: Vec<GdMember>,
    methods: Vec<GdMethod>,
    enums: Vec<GdEnum>,
    classes: Vec<GdClass>,
}

#[derive(Debug)]
pub enum GdInstruction {
    Operation(GdExpr),
    Condition(GdCondition),
    Return(GdExpr),
}

impl GdInstruction {
    pub fn condition_if(cond: GdIfCondition) -> Self {
        Self::Condition(GdCondition::If(cond))
    }
}

#[derive(Debug)]
pub enum GdUnOperation {
    UnOr(Box<GdExpr>),
    UnAnd(Box<GdExpr>),
}

#[derive(Debug)]
pub enum GdBinOperation {
    BinOr(Box<GdExpr>),
    BinAnd(Box<GdExpr>),
    Eq(Box<GdExpr>, Box<GdExpr>),
    NotEq(Box<GdExpr>, Box<GdExpr>),
    LowerThan(Box<GdExpr>, Box<GdExpr>),
    GreaterThan(Box<GdExpr>, Box<GdExpr>),
    LowerThanEqual(Box<GdExpr>, Box<GdExpr>),
    GreaterThanEqual(Box<GdExpr>, Box<GdExpr>),
}

impl GdBinOperation {
    pub fn eq(a: GdExpr, b: GdExpr) -> Self {
        Self::Eq(Box::new(a), Box::new(b))
    }
}

#[derive(Debug)]
pub struct GdEnumEntry {
    name: GdIdentifier,
    value: Option<IntType>,
}

#[derive(Debug)]
pub struct GdEnum {
    name: GdIdentifier,
    entries: Vec<GdEnumEntry>,
}

#[derive(Debug)]
pub enum GdCondition {
    If(GdIfCondition),
    While(GdConditionBranch),
}

#[derive(Debug)]
pub struct GdIfCondition {
    if_branch: GdConditionBranch,
    elif_branches: Vec<GdConditionBranch>,
    else_branch: Vec<GdInstruction>,
}

impl GdIfCondition {
    pub fn new_if(branch: GdConditionBranch) -> Self {
        Self {
            if_branch: branch,
            elif_branches: vec![],
            else_branch: vec![],
        }
    }

    pub fn then_if(mut self, branch: GdConditionBranch) -> Self {
        self.elif_branches.push(branch);
        self
    }

    pub fn then_else(mut self, block: Vec<GdInstruction>) -> Self {
        self.else_branch = block;
        self
    }
}

#[derive(Debug)]
pub struct GdConditionBranch {
    test: GdExpr,
    block: Vec<GdInstruction>,
}

#[derive(Debug)]
pub enum GdValue {
    Value(GdValueKind),
    Expr(GdExpr),
}

impl GdValue {
    pub fn int(value: IntType) -> Self {
        GdValue::Value(GdValueKind::Int(value))
    }

    pub fn identifier(value: &str) -> Self {
        GdValue::Value(GdValueKind::Identifier(GdIdentifier(value.into())))
    }
}

#[derive(Debug)]
pub enum GdValueKind {
    Int(IntType),
    Float(FloatType),
    String(String),
    Array(Vec<GdValue>),
    Object(HashMap<GdValue, GdValue>),
    Identifier(GdIdentifier),
}

#[derive(Debug)]
pub enum GdMathOperation {
    Add(Box<GdExpr>, Box<GdExpr>),
    Subtract(Box<GdExpr>, Box<GdExpr>),
    Multiply(Box<GdExpr>, Box<GdExpr>),
    Divide(Box<GdExpr>, Box<GdExpr>),
}

#[derive(Debug)]
pub enum GdExpr {
    Math(GdMathOperation),
    Bin(GdBinOperation),
    Un(GdUnOperation),
    Cast(Box<GdExpr>, GdTypeName),
    Value(Box<GdValue>),
}

impl GdExpr {
    pub fn bin_eq(a: GdExpr, b: GdExpr) -> Self {
        Self::Bin(GdBinOperation::Eq(Box::new(a), Box::new(b)))
    }

    pub fn math_add(a: GdExpr, b: GdExpr) -> Self {
        Self::Math(GdMathOperation::Add(Box::new(a), Box::new(b)))
    }

    pub fn math_multiply(a: GdExpr, b: GdExpr) -> Self {
        Self::Math(GdMathOperation::Multiply(Box::new(a), Box::new(b)))
    }

    pub fn math_subtract(a: GdExpr, b: GdExpr) -> Self {
        Self::Math(GdMathOperation::Subtract(Box::new(a), Box::new(b)))
    }

    pub fn value(value: GdValue) -> Self {
        Self::Value(Box::new(value))
    }

    pub fn int(value: IntType) -> Self {
        Self::value(GdValue::int(value))
    }

    pub fn identifier(value: &str) -> Self {
        Self::value(GdValue::identifier(value))
    }
}

#[derive(Debug)]
pub struct GdExportAttributes {}

#[derive(Debug)]
pub enum GdMemberQualifier {
    Standard,
    OnReady,
    Export(GdExportAttributes),
}

#[derive(Debug)]
pub struct GdMember {
    qualifier: GdMemberQualifier,
    name: GdIdentifier,
    gd_type: Option<GdTypeName>,
    value: Option<GdValue>,
}

#[derive(Debug)]
pub struct GdConstant {
    name: GdIdentifier,
    gd_type: Option<GdTypeName>,
    value: GdValue,
}

#[derive(Debug)]
pub enum GdMethodQualifier {
    Standard,
    Static,
}

#[derive(Debug)]
pub struct GdMethodArgument {
    name: GdIdentifier,
    gd_type: Option<GdTypeName>,
    default_value: Option<GdValue>,
}

#[derive(Debug)]
pub struct GdMethod {
    qualifier: GdMethodQualifier,
    name: GdIdentifier,
    arguments: Vec<GdMethodArgument>,
    block: Vec<GdInstruction>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code() {
        /*
         * static func hello(var1: bool, var2: int = 5):
         *     if var1:
         *         return 5
         *     elif var2 == 1:
         *         return 6
         *     elif var2 == 2:
         *         return 7
         *     else:
         *         return 8
         */
        let class_instance = GdClass {
            name: "Sample".into(),
            inherits_from: "Object".into(),
            constants: vec![],
            members: vec![],
            methods: vec![GdMethod {
                name: "hello".into(),
                qualifier: GdMethodQualifier::Static,
                arguments: vec![
                    GdMethodArgument {
                        name: "var1".into(),
                        gd_type: Some("bool".into()),
                        default_value: None,
                    },
                    GdMethodArgument {
                        name: "var2".into(),
                        gd_type: Some("int".into()),
                        default_value: Some(GdValue::int(5)),
                    },
                ],
                block: vec![GdInstruction::condition_if(
                    GdIfCondition::new_if(GdConditionBranch {
                        test: GdExpr::identifier("var1"),
                        block: vec![GdInstruction::Return(GdExpr::int(5))],
                    })
                    .then_if(GdConditionBranch {
                        test: GdExpr::bin_eq(GdExpr::identifier("var2"), GdExpr::int(1)),
                        block: vec![GdInstruction::Return(GdExpr::int(6))],
                    })
                    .then_if(GdConditionBranch {
                        test: GdExpr::bin_eq(GdExpr::identifier("var2"), GdExpr::int(2)),
                        block: vec![GdInstruction::Return(GdExpr::int(7))],
                    })
                    .then_else(vec![GdInstruction::Return(GdExpr::int(8))]),
                )],
            }],
            enums: vec![],
            classes: vec![],
        };
    }

    #[test]
    fn test_expr() {
        // 1 + 2 + (3 + 4) * 6 - var
        let expr = GdExpr::math_add(
            GdExpr::int(1),
            GdExpr::math_add(
                GdExpr::int(2),
                GdExpr::math_subtract(
                    GdExpr::math_multiply(
                        GdExpr::math_add(GdExpr::int(3), GdExpr::int(4)),
                        GdExpr::int(6),
                    ),
                    GdExpr::identifier("var"),
                ),
            ),
        );
    }
}
