"
I am a base class for commands which perform operations with particular ast-node of given method.
  
Internal Representation and Key Implementation Points.

    Instance Variables
	method:		<CompiledMethod>
	sourceNode:		<ASTProgramNode>
"
Class {
	#name : 'SycSourceCodeCommand',
	#superclass : 'CmdCommand',
	#traits : 'TRefactoringCommandSupport',
	#classTraits : 'TRefactoringCommandSupport classTrait',
	#instVars : [
		'method',
		'sourceNode'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycSourceCodeCommand class >> isAbstract [
	^self = SycSourceCodeCommand
]

{ #category : 'instance creation' }
SycSourceCodeCommand class >> method: aMethod node: aProgramNode [
	^self new
		method: aMethod;
		sourceNode: aProgramNode
]

{ #category : 'converting' }
SycSourceCodeCommand >> asRefactorings [
	self subclassResponsibility
]

{ #category : 'execution' }
SycSourceCodeCommand >> execute [

	self executeRefactoring: self asRefactorings
]

{ #category : 'private' }
SycSourceCodeCommand >> executeRefactoring: refactoring [

	self initializeDefaultOptionsOf: refactoring.
	[ refactoring execute ]
		on: RBRefactoringError
		do: [ :e | self morphicUIManager alert: e messageText ]
]

{ #category : 'accessing' }
SycSourceCodeCommand >> method [
	^ method
]

{ #category : 'accessing' }
SycSourceCodeCommand >> method: anObject [
	method := anObject
]

{ #category : 'execution' }
SycSourceCodeCommand >> readParametersFromContext: aSourceCodeContext [
	super readParametersFromContext: aSourceCodeContext.

	method := aSourceCodeContext lastSelectedMethod.
	sourceNode := aSourceCodeContext lastSelectedSourceNode
]

{ #category : 'accessing' }
SycSourceCodeCommand >> sourceNode [
	^ sourceNode
]

{ #category : 'accessing' }
SycSourceCodeCommand >> sourceNode: anObject [
	sourceNode := anObject
]
