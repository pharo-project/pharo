"
This is a custom help book for the refactoring framework.
"
Class {
	#name : #RefactoringHelp,
	#superclass : #CustomHelp,
	#category : #'Refactoring-Help'
}

{ #category : #pages }
RefactoringHelp class >> about [
	^HelpTopic
		title: 'About'
		contents: 
'The Refactoring Engine was originally developed by Don Roberts and John Brant for VisualWorks. It was ported to squeak and pharo with extensions by Lukas Renggli and other contributors.

One goal of this framework was to easily include the code refactoring into the standard development work flow. The refactoring operations should help to transform and restructure the source code, without to much manual intervention. And without the need to retest every single change.

Therefore some basic primitive refactoring are provided, in a way that more complex operations can be constructed by compositing the primitive ones. 

A refactoring tool should provide an environment for viewing the code, the refactoring transformation and the result. Originally a standalone tool, later the support for refactoring operations were directly integrated into the standard Smalltalk System Browser.

The Tool, or a browser with refactoring support or the whole framework is often just known as the ''Refactoring Browser''. 

That''s why all of the classes of this framework start with the prefix ''RB''.
'
]

{ #category : #accessing }
RefactoringHelp class >> bookDescription [
	^ self comment
]

{ #category : #accessing }
RefactoringHelp class >> bookName [
	^ 'Refactoring Framework'
]

{ #category : #pages }
RefactoringHelp class >> coreComponents [
	^HelpTopic
		title: 'Core Components'
		contents: 
'
A brief overview of the core components.

RBScanner and RBParser
The Refactoring Framework has its own parser and scanner, creating an abstract syntax tree (AST) from the methods source code.

RBProgramNode and subclasses
These are the base and concrete subclasses for all RB-Nodes representing a syntax node class, like RBMethodNode, RBAssignmentNode, RBBlockNode et cetera.

RBParseTreeSearcher and RBParseTreeRewriter
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
]

{ #category : #pages }
RefactoringHelp class >> overview [
	^HelpTopic
		title: 'Overview'
		contents: 
'
The following pages give an overview of the refactoring engine, the collaborating classes and components.

The refactoring support in the System Browser with some explanations of the different refactoring operations.

It follows an explanation with some examples, how to manually construct and execute refactoring operations, usable for those operation that aren''t supported as operations in the System Browser or to help to include them in your own tools.

A more in-deep description of the components shows that the refactoring frameworks is not only useful for code refactoring, but it also provides a powerful general purpose search and rewrite engine. 

This search capability - search for code "patterns" - can be used to detect common program errors or just bad code style. The code structures are defined as a search patterns and build a Lint-rule set for static code analysis.
'
]

{ #category : #accessing }
RefactoringHelp class >> pages [
	^#( about overview coreComponents RBBrowserSupportHelp RBRefactoringExamplesHelp RBRefactoringCoreHelp)
]
