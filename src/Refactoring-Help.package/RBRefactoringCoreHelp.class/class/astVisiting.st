pages
astVisiting
	^HelpTopic
		title: 'AST Vistor'
		contents: 
'
With this hierarchy of classes, the operations and programs working with the AST are often implemented with the visitor pattern.

AST node visitors are subclasses of a ProgramNodeVisitor, or a just any other class implementing the appropriate visitNode: / visitXXX: methods.

Some examples of ProgramNodeVisitors operating on the RBParsers AST:

Opal Compiler
Opals translator visits the AST tree to create a intermediate representation that is finally used to generated method byte code. Another step in the compiler work flow, the ClosureAnalyzer, is implemented as
a ProgramNodeVisitor too.

Reflectivity Compiler
For reflectivity support, can add MetaLinks to the nodes of the compiled method and generate new methods with code injections augmenting or modifying the executed code.

Code formatter (BIConfigurableFormatter/BISimpleFormatter)
A code formatter walks over the AST tree and reformats the code (node positions) based on a simple format rule or a configurable formatting style.

TextStyler
SHRBTextStyler builds a attributed text representation of the source code, augmented with text font, color or emphasis attributes based on the current style settings. 

And of course
RBParseTreeSearcher and RBParseTreeRewriter
The original users of this AST structure for searching and rewriting code, more on this in its own chapter.
'
