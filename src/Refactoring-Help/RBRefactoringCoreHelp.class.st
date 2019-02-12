"
A chapter with a more in-depth description of the core components of the refactoring engine.
"
Class {
	#name : #RBRefactoringCoreHelp,
	#superclass : #RefactoringHelp,
	#category : #'Refactoring-Help'
}

{ #category : #pages }
RBRefactoringCoreHelp class >> astContent [
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
]

{ #category : #pages }
RBRefactoringCoreHelp class >> astVisiting [
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

]

{ #category : #accessing }
RBRefactoringCoreHelp class >> bookName [
	^ 'RB Refactoring Engine'
]

{ #category : #pages }
RBRefactoringCoreHelp class >> browserEnvironments [
	^HelpTopic
		title: 'RBBrowserEnvironment'
		contents: 
'
The first and main use for browser environments are to restrict the namespace in which a refactoring operation is applied. For example, if you want to rename a method and and update all senders of this method, but only in a certain package, you can create a RBNamespace from a scoped ''view'' of the classes from the whole system. Only the classes in this restricted environment are affected by the transformation.

In the mean time other tools are using this environment classes as well. Finder, MessageBrowser or the SystemBrowser can work with a scoped environment to show and operate only on classes and methods in this environment.

There are different subclasses of RBBrowserEnvironment for the different kind of ''scopes''. 

RBClassEnvironment - only show classes/methods from a set of classes.
RBPackageEnvironment - only show classes / packages / methods from a set of packages.
RBSelectorEnvironment - only show classes / methods from a set of selector names.
(see the list of RBEnvironment(subclasses) pages in this book).

Instead of directly using the different subclasses for a scoped view, the base class RBBrowserEnvironment can act as a factory for creating restricted environments. See the methods in its ''environments''-protocol, on how to create the different environments.

You start with a default environment containing all classes from the system and create a new scoped environment by calling the appropriate method.

For example, creating an environment for all classes in package ''Kernel'':

RBBrowserEnvironment new forPackageNames:{''Kernel''}.

You can query the environment just like you for Smalltalk globals
|env|
env := RBBrowserEnvironment new forPackageNames:{''Kernel''}.
env allClasses "-> a list of all classes in package Kernel"

or open a browser
env browse "-> starts Calypso showing only this package"

and you can further restrict this package environment by calling one of the other factory methods:

env class "-> a RBPackageEnvironment"
(env implementorsOf:#collect:) class "->  RBSelectorEnvironment"

Another way to combine or further restrict environments is to use boolean operations and, not or or.

|implDrawOn callsDrawOn implAndCalls |
callsDrawOn := RBBrowserEnvironment new referencesTo: #drawOn:.
implDrawOn :=  RBBrowserEnvironment new implementorsOf: #drawOn:.
"create an ''anded''-environment"
implAndCalls := callsDrawOn & implDrawOn.
"collect all message and open a MessageBrowser"
MessageBrowser browse: implAndCalls methods.

This opens a MessageBrowser on all methods in the system that implement #drawOn: and calls drawOn:.

|implPrintOn notImplPrintOn |
implPrintOn := RBBrowserEnvironment new implementorsOf: #printOn:.
"create a ''not''-environment"
notImplPrintOn := implPrintOn not.
implPrintOn includesClass: Object. "-> true"
notImplPrintOn includesClass: Object. "-> false"

classes implementing #printOn: are not in the ''not''-environment.

A more generic way to create an environment by giving an explicit ''test''-block to select methods for this environment:

|implementedByMe|
implementedByMe := RBBrowserEnvironment new selectMethods:[:m | m author = Author fullName ].
implementedByMe browse.
 
This opens (may be slow) a browser with all classes with methods having my (current Author) name for its current methods version author stamp.
'
]

{ #category : #pages }
RBRefactoringCoreHelp class >> overview [
	^HelpTopic
		title: 'Overview'
		contents: 
'
This book contains some chapter about the core components
the Abstract Syntax Tree (AST)
the parser (RBParser)
the extended pattern parser (RBPatternParser)
the tree searcher / rewriter (RBParseTreeSearcher/RBParseTreeRewriter)
'
]

{ #category : #pages }
RBRefactoringCoreHelp class >> overviewParser [
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
]

{ #category : #accessing }
RBRefactoringCoreHelp class >> pages [
	^ #(overview astContent astVisiting overviewParser patternParser patternParserExamples browserEnvironments)
]

{ #category : #pages }
RBRefactoringCoreHelp class >> patternParser [
	^HelpTopic
		title: 'RBPatternParser and metavariables'
		contents: 
'
Generating an AST of Smalltalk source code and implementing a program node visitor gives already great and powerful capabilities. The refactoring framework extends this expressiveness by including so called "metavariables".

As this expressions are using an extended syntax - metavariables aren''t known to the RBParser - a special parser is needed to parse this expression, the RBPatternParser.
The following pages describe the added syntax elements. Examples on how to use or tests these expressions
can be found in the chapter "RBPatternParser examples".

metavariables are a part of a parser expression, just like any other Smalltalk code, but instead of representing an expression with the exact name, they form a variable that can be unify with any real code expression with the same *structure*.

An example:
Parsing an expression like:
a := a + 1 
creates a parse tree with an assignment node assigning to ''a'', the value of sending the message ''+'' with argument 1 to the object ''a''.

We could implement a refactoring operation (or directly use the RBParseTreeSearcher/Rewriter) to create a refactoring  for this kind of code. But of course, it would only work for code using this variable name.

We can define the expression with the meaning of ''increment a variable by one'' by using a metavariable. All metavariables start with a ´ (backquote).
`a := `a + 1

This is the simplest metavariable, a name with a backquote. It will match a single variable. And for matching the whole expression, all variables with the same name must match the same variables. 
The above expression only matches 
''x:=x+1'' 
but not 
''x:=y+1''.

If we want to match more than a single variable, we can prefix the name with a ''@'':

`a matches a single variable
`@a matches multiple items in this position

For example, 
`@a add: `@b
will match any expression with the message send #add: regardless whether the receiver or arguments are single variables
''coll add: item''
or the return of another expression
''self data add: self''

Furthermore we can restrict the expression to be matched to be a literal instead of variable by using the prefix ''#'':

`@exp add: `#item

This will match any code calling #add: on an object or expression with a literal as argument:
''coll add: 3''
''self foo add: ''text'' ''
''coll add: #symbol''

But again, #lit is a named variable and matches only the same literal in every part of the expression:

`self add: `#lit; add: `#lit

will match
''self add: #a; add: #a''
but not 
''self add: #a; add: #b''

Similar to a statement ending with a dot, the metavariable prefix ''.'' defines a variable matching a statement, resp. ''.@'' a (possible empty) list of statements.

Example, match ifTrue:ifFalse: with first statement in true and false block being the same

`@exp ifTrue:[`.stm. 
				  `.@trueCase]
      ifFalse:[`.stm. 
				  `.@falseCase]

This will match

someValue ifTrue:[ self doA.
	                self doFoo]
          ifFalse:[ self doA.
	                self doBaz]


Important especially for the rewriter, we may not only want to know the first node matching an expression but every other and for example any possible subexpression matching the metavariable. For this, we can
use a double backquote to indicate that the search should recurse into the found node expression to search for more matches.

This expression will find all senders of add:
`@exp add:`@value
but if we would use this expression to rewrite add: by addItem:
an expression like

var add: (self add: aValue).

would be replaced by

var addItem: (self add: aValue).

If we want to find the same call in the argument, we need to recurse into it by using a double backquote

`@exp add:``@value
'
]

{ #category : #pages }
RBRefactoringCoreHelp class >> patternParserExamples [
	^HelpTopic
		title: 'Examples and usage of RBPatternParser expressions'
		contents: 
'
The chapter "RBPatternParser and metavariables" describes the added syntax elements for the RBPatternParser used in the refactoring engine (RBParseTreeSearcher/RBParseTreeRewriter).

In this chapter we show some example expressions and how to test and use them.

Calypso has a search function that is the simples way to use and see the result of searching expressions with pattern syntax. Open the the class menu / Refactoring / Code Rewrite / Search code or Rewrite code entry.

Search code
The search code menu will put a search pattern template in the code pane:

RBParseTreeSearcher new
	matches: ''`@object'' do: [ :node :answer | node ];
	matchesMethod: ''`@method: `@args | `@temps | `@.statements'' do: [ :node :answer | node ];
	yourself
	

This template defines two match rules, one for the code search ''matches:'' and one for the named method search ''matchesMethod'', the former looks for expression in any method while the latter one matches whole methods.

You can replace the example pattern ''`@object'' or ''`@method: `@args | `@temps | `@.statements'' by
the search pattern you want to use. And most of the time you only want to use one, the code expression search or the method search.

A first example, replace the code pane content by:
RBParseTreeSearcher new
	matchesMethod: ''drawOn: `@args | `@temps | `@.statements'' do: [ :node :answer | node ];
	yourself

You can now accept this code, instead of saving this method it will just spawn a code searcher trying all defined methods to match against this pattern and opens a MessageBrowser for all found results.
The result is actually the same as if we had searched for all implementors of #drawOn:

Next example, replace the code pane content by:
RBParseTreeSearcher new
	matches: ''`@object drawOn: `@args'' do: [ :node :answer | node ];
	yourself

The result is similar to looking for senders of #drawOn: (not the same actually, as sendersOf also looks for methods containing the symbol #drawOn: )	
	
The #do: block can be used to further test or filter the found matches. The node is the current matched node and the answer is not needed here. It is important that for every entry you want to include in the result to return "the node" and for everything else return "nil"

Example, search for all methods with at least one argument where the method name starts with ''permform'':

RBParseTreeSearcher new
		matchesMethod: ''`@method: `@args | `@temps | `@.statements''
			do: [ :node :answer | 
			((node selector beginsWith: ''perform'') and: [ node arguments isEmpty not ])
				ifTrue: [ node ]
				ifFalse: [ nil ] ];
		yourself

Another way to use extended pattern syntax is to directly instantiate a RBParseTreeSearcher and execute it on a parse tree.
First we define the pattern, instantiate a tree searcher and tell him what to do when matching this pattern (just return the matched node) and execute it on the AST of Numbers method #asPoint.

| searcher pattern parseTree |
pattern := ''^ self''.
searcher := RBParseTreeSearcher new.
searcher matches: pattern do:[:node :answer |node].
searcher executeTree: (Number>>#asPoint) ast initialAnswer: nil.

it will return nil, since no node in that method returns ''self''. If we execute the searcher instead on the method
for class Point, it will return the found node, a RBReturnNode

searcher executeTree: (Point>>#asPoint) ast initialAnswer: nil.

If we don''t just want to match an expression but collecting all matching nodes, we can collect all nodes within the #do: block:

| searcher pattern parseTree  selfMessages |
selfMessages := Set new.
pattern := ''self `@message: ``@args''.
searcher := RBParseTreeSearcher new.
searcher matches: pattern do:[:node :answer |  selfMessages add: node selector].
searcher executeTree: (Morph>>#fullDrawOn:) ast initialAnswer: nil.
selfMessages inspect.

This will collect all messages send to self in method Morph>>#fullDrawOn:


'
]
