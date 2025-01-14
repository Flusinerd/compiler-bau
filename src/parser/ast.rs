use std::fmt;

#[derive(Debug)]
pub enum ASTNode {
    Program(Vec<ASTNode>), // The root node
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

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTNode::Program(nodes) => {
                writeln!(f, "Program {{")?;
                for node in nodes {
                    writeln!(f, "  {}", node)?;
                }
                writeln!(f, "}}")
            }
            ASTNode::Function {
                name,
                params,
                return_type,
                body,
            } => {
                writeln!(
                    f,
                    "Function {}({}) -> {} {{",
                    name,
                    params
                        .iter()
                        .map(|(typ, name)| format!("{} {}", typ, name))
                        .collect::<Vec<_>>()
                        .join(", "),
                    return_type
                )?;
                for stmt in body {
                    writeln!(f, "  {}", stmt)?;
                }
                writeln!(f, "}}")
            }
            ASTNode::VariableDeclaration {
                var_type,
                name,
                value,
            } => {
                write!(f, "let {} {} = {}", var_type, name, value)
            }
            ASTNode::BinaryOperation {
                operator,
                left,
                right,
            } => {
                write!(f, "({} {} {})", left, operator, right)
            }
            ASTNode::Identifier(name) => write!(f, "{}", name),
            ASTNode::Number(value) => write!(f, "{}", value),
            ASTNode::Return(value) => write!(f, "return {}", value),
        }
    }
}
