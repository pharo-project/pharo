pages
overviewParser
	^HelpTopic
		title: 'RBParser'
		contents: 
'
The Refactoring Framework contains its own parser.

Defining or implementing refactoring operations on the raw source code level is difficult. For example, we would have to distinguish whether a word is an instance variable name, an argument or a reserved word.
Therefor a parser first translates the source code into an abstract syntax tree (AST).

The tree consists of nodes for every source code element, tagged it with some "type" information (the node subclass), source code location, and optional properties. And it represents the whole source code structure. 

For example, the AST for the source code of a method has a RBMethodNode with child nodes RBArgument for the arguments (if any) and a RBSequenceNode for the code body. The RBSequenceNode has child nodes for any
defined temporaries and the actual code, RBAssignmentNode for variable assignments, RBMessageNode for message sends.

This is how the structure  for Numbers #sgn method AST looks:
RBParser parseMethod:''sign
	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0''

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

Although many Smalltalk implementations already include a parser as a part of its compiler tool chain, they don''t fulfill the requirements needed for the code transformations with the refactoring framework.
The AST for the compiler, is often only needed to create the byte code and therefore can ignore any code comments or the code formatting. If we use the AST in the refactoring for search and replace code, for example renaming a variable, we don''t want to reformat the whole code or remove any code comments. 

The RBParser therefore stores the original code locations and code comments, and only replaces those elements defined by the refactoring transformation and preserves the method comments.

In recent pharo versions, the RBParser actual replaces the original parser used to compile code. It is as powerful as the prior parser, maybe a little bit slower, but easier to maintain. And in the mean time other tools, despite the compiler and the refactoring framework are using this tools as well. 
(For instance, the syntax highlighting and the code formatter are based on the RBParsers AST nodes).

But the real strength of the refactoring framework comes from another (RBParser sub-) class, the 
RBPatternParser, described in its own chapter.
'