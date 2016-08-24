pages
astContent
	^HelpTopic
		title: 'AST Nodes'
		contents: 
'
The AST representing the code by a tree of nodes. A node may represent 
a single element
- RBVariableNode 
- RBLiteralValueNode 
an expression
- RBAssignmentNode
- RBMessageNode
- RBReturnNode
- RBCascadeNode
a sequence of expressions
- RBSequenceNode
or a block or Method
- RBBlockNode
- RBMethodNode

This nodes are part of a class hierarchy starting with RBProgramNode an abstract class defining the common operations needed for all nodes. Every node knows about its child nodes, the source code location, any comment attached (comment prior to this node in the source code, or for RBMethodNodes the "method comment" line), and the type (by its subclass) - see the is-Methods in "testing"-protocol.

Keep in mind that the syntax tree is created from the source code only and may not distinguish all possible type information without actually analyzing the semantic context. For example, a global variable is represented as RBGlobalNode, but just from parsing an expression, the AST only knows that this is a RBVariableNode. You need to call doSemanticAnalysis on the parse tree to convert variable nodes into the  type they represent in the code.

'