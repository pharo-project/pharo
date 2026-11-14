"
I am a base class for commands which perform operations with collection of methods.

Internal Representation and Key Implementation Points.

    Instance Variables
	methods:		<Colletion of<CompiledMethod>>
"
Class {
	#name : 'SycMethodCommand',
	#superclass : 'ClyCommand',
	#traits : 'TRefactoringCommandSupport',
	#classTraits : 'TRefactoringCommandSupport classTrait',
	#instVars : [
		'methods'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'instance creation' }
SycMethodCommand class >> for: methods [
	^self new
		methods: methods
]

{ #category : 'testing' }
SycMethodCommand >> canBeExecuted [

	^ self context isMethodSelected
]

{ #category : 'accessing' }
SycMethodCommand >> methods [

	^ methods ifNil: [
			  self hasContext
				  ifTrue: [ self context selectedMethods ]
				  ifFalse: [ #(  ) ] ]
]

{ #category : 'accessing' }
SycMethodCommand >> methods: anObject [
	methods := anObject
]

{ #category : 'execution' }
SycMethodCommand >> prepareFullExecution [

	methods := self context selectedMethods
]
