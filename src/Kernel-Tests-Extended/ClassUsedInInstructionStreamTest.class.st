Class {
	#name : #ClassUsedInInstructionStreamTest,
	#superclass : #SuperClassUsedInInstructionStreamTest,
	#instVars : [
		'expectedValue',
		'aProcess'
	],
	#category : #'Kernel-Tests-Extended-Methods'
}

{ #category : #examples }
ClassUsedInInstructionStreamTest >> aMethodSuspendedBeforeTheTerminateOfAnotherProcess [

	Processor activeProcess suspend.

	aProcess terminate.
	
	expectedValue := 42.

]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> aMethodWithHalt [

	<haltOrBreakpointForTesting>

	Processor activeProcess suspend.
	self halt.
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> aMethodWithMNU [

	Processor activeProcess suspend.
	self iAmAnMNUMessage.
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> aMethodWithMustBeBooleanMNU [

	Processor activeProcess suspend.

	^ 2 ifTrue: [ 5 ] ifFalse: [ 7 ]
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> aMethodWithSuspendAndReturnANumber [

	Processor activeProcess suspend.
	^ 42.
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> aSuperMethod: anInteger with: anotherInteger [

	^ self error
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> callingAMethodSuspendedBeforeTheTerminateOfAnotherProcess [

	self aMethodSuspendedBeforeTheTerminateOfAnotherProcess
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> callingAMethodWithHalt [

	self aMethodWithHalt
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> callingAMethodWithMNU [

	self aMethodWithMNU
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> callingAMethodWithMustBeBooleanMNU [

	self aMethodWithMustBeBooleanMNU
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> callingAMethodWithSuspendAndReturnANumber [

	^ self aMethodWithSuspendAndReturnANumber
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> expectedValue [

	^ expectedValue
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> methodWithASuperBlock [

	^ [ super aSuperMethod: 1 with: 2 ]
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> methodWithASuperBlockWithoutArguments [

	^ [ super yourself ]
]

{ #category : #accessing }
ClassUsedInInstructionStreamTest >> process: aValue [ 
	
	aProcess := aValue
]

{ #category : #examples }
ClassUsedInInstructionStreamTest >> valueOfBlockWithSupersend [

	expectedValue := self methodWithASuperBlock value
]
