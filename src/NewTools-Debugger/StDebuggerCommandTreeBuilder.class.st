"
I am an abstract class which provides interfaces for building a tree of commands for the StDebugger.
I set the debugger instance and the root command group on which the tree is going to be built, and instantiation interface on class side.
My subclasses must implement buildDebugerCommandGroup in which they build their particular subgroup of commands.
My subclasses know how to inject their subgroup into the main command tree.  

My subclasses are responsible to provide groups of commands classes that are instantiated on the given context and presenter. 

For instance: 
- an ""accessing"" protocol with methods returning names of groups
- an ""accessing-command classes"" protocol returning arrays of methods (i.e., groups)
"
Class {
	#name : #StDebuggerCommandTreeBuilder,
	#superclass : #Object,
	#instVars : [
		'rootCommandGroup',
		'stDebuggerInstance'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #commands }
StDebuggerCommandTreeBuilder class >> buildCommandsGroupWith: aStDebuggerInstance forRoot: aRootCommandGroup [
	^self new buildCommandsGroupWith: aStDebuggerInstance forRoot: aRootCommandGroup
]

{ #category : #accessing }
StDebuggerCommandTreeBuilder class >> displayStrategy [
	^self subclassResponsibility 
]

{ #category : #accessing }
StDebuggerCommandTreeBuilder class >> groupDescription [
	^self subclassResponsibility 
]

{ #category : #accessing }
StDebuggerCommandTreeBuilder class >> groupName [
	^self subclassResponsibility 
]

{ #category : #converting }
StDebuggerCommandTreeBuilder >> asMenuPresenter [ 	
	^rootCommandGroup asMenuPresenter
]

{ #category : #building }
StDebuggerCommandTreeBuilder >> buildAndRegisterGroup [
	"builds the group that will register under the provided root command group"

	| group |
	group := (CmCommandGroup named: self groupName) asSpecGroup.
	group description: self groupDescription.
	group displayStrategy: self displayStrategy.
	rootCommandGroup register: group.
	^ group
]

{ #category : #building }
StDebuggerCommandTreeBuilder >> buildCommandsGroupWith: aStDebuggerInstance forRoot: aRootCommandGroup [
	stDebuggerInstance := aStDebuggerInstance.
	rootCommandGroup := aRootCommandGroup.
	self buildDebuggerCommandGroup
]

{ #category : #building }
StDebuggerCommandTreeBuilder >> buildDebuggerCommandGroup [
	self subclassResponsibility 
]

{ #category : #building }
StDebuggerCommandTreeBuilder >> buildSpecCommand: aCommandClass forContext: anObject [
	^ aCommandClass forSpecContext: anObject
]

{ #category : #accessing }
StDebuggerCommandTreeBuilder >> displayStrategy [
	^self class displayStrategy new
]

{ #category : #accessing }
StDebuggerCommandTreeBuilder >> group [
	^ rootCommandGroup
		commandOrGroupNamed: self groupName
		ifNone: [ self buildAndRegisterGroup ]
]

{ #category : #accessing }
StDebuggerCommandTreeBuilder >> groupDescription [
	^self class groupDescription
]

{ #category : #accessing }
StDebuggerCommandTreeBuilder >> groupName [ 
	^self class groupName
]
