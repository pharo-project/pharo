Class {
	#name : #ClassUsedInInstructionStreamTest,
	#superclass : #SuperClassUsedInInstructionStreamTest,
	#instVars : [
		'expectedValue',
		'aProcess'
	],
	#category : #'Kernel-Tests-Extended-Methods'
}

{ #category : #example }
ClassUsedInInstructionStreamTest >> aMethodSuspendedBeforeTheTerminateOfAnotherProcess [

	Processor activeProcess suspend.

	aProcess terminate.
	
	expectedValue := 42.

]

{ #category : #example }
ClassUsedInInstructionStreamTest >> aMethodWithHalt [

	Processor activeProcess suspend.
	self halt.
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> aMethodWithMNU [

	Processor activeProcess suspend.
	self iAmAnMNUMessage.
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> aMethodWithMustBeBooleanMNU [

	Processor activeProcess suspend.

	^ 2 ifTrue: [ 5 ] ifFalse: [ 7 ]
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> aMethodWithSuspendAndReturnANumber [

	Processor activeProcess suspend.
	^ 42.
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> aSuperMethod: anInteger with: anotherInteger [

	^ self error
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> callingAMethodSuspendedBeforeTheTerminateOfAnotherProcess [

	self aMethodSuspendedBeforeTheTerminateOfAnotherProcess
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> callingAMethodWithHalt [

	self aMethodWithHalt
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> callingAMethodWithMNU [

	self aMethodWithMNU
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> callingAMethodWithMustBeBooleanMNU [

	self aMethodWithMustBeBooleanMNU
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> callingAMethodWithSuspendAndReturnANumber [

	^ self aMethodWithSuspendAndReturnANumber
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> expectedValue [

	^ expectedValue
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> methodWithASuperBlock [

	^ [ super aSuperMethod: 1 with: 2 ]
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> methodWithASuperBlockWithoutArguments [

	^ [ super yourself ]
]

{ #category : #accessing }
ClassUsedInInstructionStreamTest >> process: aValue [ 
	
	aProcess := aValue
]

{ #category : #example }
ClassUsedInInstructionStreamTest >> valueOfBlockWithSupersend [

	expectedValue := self methodWithASuperBlock value
]
