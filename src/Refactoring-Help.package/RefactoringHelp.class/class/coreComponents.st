pages
coreComponents
	^HelpTopic
		title: 'Core Components'
		contents: 
'
A brief overview of the core components.

RBScanner and RBParser
The Refactoring Framework has its own parser and scanner, creating an abstract syntax tree (AST) from the methods source code.

RBProgramNode and subclasses
These are the base and concrete subclasses for all RB-Nodes representing a syntax node class, like RBMethodNode, RBAssignmentNode, RBBlockNode et cetera.

RParseTreeSearcher and RBParseTreeRewriter
A refactoring operation uses the tree searcher and rewriter for applying a transformation on the abstract syntax tree.
They implement a program node visitor.

RBClass, RBMetaclass, RBMethod
Class and Method meta-models representing a class or method created, removed or modified during a refactoring
operation.

RBNamespace
A namespace is an environment for resolving class and method entities by name and collects all changes resp. changed entities.

RBRefactoring and subclasses
Abstract base classes and its concrete subclasses for refactoring operations. Every basic refactoring operation is implemented as a subclass of the RBRefactoring class. A refactoring operation checks the precondition that must be fulfilled and implements the actual code transform method.

RBCondition
Instead of implementing conditions and condition checking code into every single refactoring operation, the RBCondition class implements a set
of common tests and can be created and combined to realize a composition of conditions.

RBRefactoryChange
Applying a refactoring within a namespace collects changes without applying the actual change to the system. This changes are represented
by a RBRefactoryChange, its subclass or a composition of refactory changes. 

'