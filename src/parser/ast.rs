#[derive(Debug)]
pub enum ASTNode {
    Program(Vec<ASTNode>),         // The root node
    Function {
        name: String,
        params: Vec<(String, String)>, // (type, name)
        return_type: String,
        body: Vec<ASTNode>,
    },
    VariableDeclaration {
        var_type: String,
        name: String,
        value: Box<ASTNode>,
    },
    BinaryOperation {
        operator: char,
        left: Box<ASTNode>,
        right: Box<ASTNode>,
    },
    Identifier(String), // Variable or function names
    Number(String),     // Numbers as strings to handle int64/double
    Return(Box<ASTNode>),
}
