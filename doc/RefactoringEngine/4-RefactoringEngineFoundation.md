# Refactoring Engine Foundation

A chapter with a more in-depth description of the core components of the refactoring engine.

### Overview

This book contains some chapter about the core components

- the Abstract Syntax Tree (AST)
- the parser (`RBParser`)
- the extended pattern parser (`RBPatternParser`)
- the tree searcher / rewriter (`RBParseTreeSearcher`/`RBParseTreeRewriter`)

### AST Nodes

The AST representing the code by a tree of nodes. A node may represent 
a single element
- ASTVariableNode 
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

These nodes are part of a class hierarchy starting with ASTProgramNode an abstract class defining the common operations needed for all nodes. Every node knows about its child nodes, the source code location, any comment attached (comment prior to this node in the source code, or for RBMethodNodes the "method comment" line), and the type (by its subclass) - see the is-Methods in "testing"-protocol.

Keep in mind that the syntax tree is created from the source code only and may not distinguish all possible type information without actually analyzing the semantic context. For example, a global variable is represented as RBGlobalNode, but just from parsing an expression, the AST only knows that this is a RBVariableNode. You need to call doSemanticAnalysis on the parse tree to convert variable nodes into the  type they represent in the code.

### AST Vistor

With this hierarchy of classes, the operations and programs working with the AST are often implemented with the visitor pattern.

AST node visitors are subclasses of a ProgramNodeVisitor, or a just any other class implementing the appropriate visitNode: / visitXXX: methods.

Some examples of ProgramNodeVisitors operating on the RBParsers AST:

- Opal Compiler. Opals translator visits the AST tree to create an intermediate representation that is finally used to generated method byte code. Another step in the compiler work flow, the ClosureAnalyzer, is implemented as
a `ProgramNodeVisitor` too.
- Reflectivity Compiler. For reflectivity support, can add MetaLinks to the nodes of the compiled method and generate new methods with code injections augmenting or modifying the executed code.
- Code formatter. (BIConfigurableFormatter/BISimpleFormatter) A code formatter walks over the AST tree and reformats the code (node positions) based on a simple format rule or a configurable formatting style.
- TextStyler. SHRBTextStyler builds an attributed text representation of the source code, augmented with text font, color or emphasis attributes based on the current style settings.
- `RBParseTreeSearcher` and `RBParseTreeRewriter`. The original users of this AST structure for searching and rewriting code, more on this in its own chapter.

### RBParser

Defining or implementing refactoring operations on the raw source code level is difficult. For example, we would have to distinguish whether a word is an instance variable name, an argument or a reserved word.
Therefor a parser first translates the source code into an abstract syntax tree (AST).

The tree consists of nodes for every source code element, tagged it with some "type" information (the node subclass), source code location, and optional properties. And it represents the whole source code structure. 

For example, the AST for the source code of a method has a RBMethodNode with child nodes RBArgument for the arguments (if any) and a RBSequenceNode for the code body. The RBSequenceNode has child nodes for any
defined temporaries and the actual code, RBAssignmentNode for variable assignments, RBMessageNode for message sends.

This is how the structure for Numbers #sgn method AST looks:

```st
RBParser parseMethod:'sign
	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0'
```
```
|->RBMethodNode sign
  |->RBSequenceNode self > 0 ifTrue: [ ^ 1 ]. self < 0 ifTrue: [ ^ -1 ]. ^ 0
    |->RBMessageNode ifTrue:
      |->RBMessageNode >
        |->RBSelfNode self
        |->RBLiteralValueNode 0
      |->RBBlockNode [ ^ 1 ]
        |->RBSequenceNode ^ 1
          |->RBReturnNode ^ 1
            |->RBLiteralValueNode 1
    |->RBMessageNode ifTrue:
      |->RBMessageNode <
        |->RBSelfNode self
        |->RBLiteralValueNode 0
      |->RBBlockNode [ ^ -1 ]
        |->RBSequenceNode ^ -1
          |->RBReturnNode ^ -1
            |->RBLiteralValueNode -1
    |->RBReturnNode ^ 0
      |->RBLiteralValueNode 0
```

The AST for the compiler, is often only needed to create the byte code and therefore can ignore any code comments or the code formatting. If we use the AST in the refactoring for search and replace code, for example renaming a variable, we don't want to reformat the whole code or remove any code comments. 

The RBParser therefore stores the original code locations and code comments, and only replaces those elements defined by the refactoring transformation and preserves the method comments.

In recent pharo versions, the RBParser actual replaces the original parser used to compile code. It is as powerful as the prior parser, maybe a little bit slower, but easier to maintain. And in the mean time other tools, despite the compiler and the refactoring framework are using this tools as well. (For instance, the syntax highlighting and the code formatter are based on the RBParsers AST nodes).

But the real strength of the refactoring framework comes from another (RBParser sub-) class, the 
`RBPatternParser`, described in its own chapter.
