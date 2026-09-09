Class {
	#name : 'InstructionStreamSteppingTest',
	#superclass : 'TestCase',
	#instVars : [
		'operations'
	],
	#category : 'Debugger-Model-Tests-Core',
	#package : 'Debugger-Model-Tests',
	#tag : 'Core'
}

{ #category : 'helpers' }
InstructionStreamSteppingTest >> blockReturnTop [

	operations add: { #blockReturnTop }
]

{ #category : 'helpers' }
InstructionStreamSteppingTest >> classUnderTest [

	^ ClassUsedInInstructionStreamTest
]

{ #category : 'helpers' }
InstructionStreamSteppingTest >> directedSuperSend: aString numArgs: anInteger [

	operations add: { #directSuperSend:numArgs:. aString. anInteger }
]

{ #category : 'accessing' }
InstructionStreamSteppingTest >> pc: anInteger [

	"Ignore the PC"
]

{ #category : 'helpers' }
InstructionStreamSteppingTest >> pushConstant: anInteger [

	operations add: { #pushConstant. anInteger }
]

{ #category : 'helpers' }
InstructionStreamSteppingTest >> pushLiteralVariable: aLiteralVariable [

	operations add: { #pushConstant. aLiteralVariable }
]

{ #category : 'helpers' }
InstructionStreamSteppingTest >> pushReceiver [

	operations add: {#pushReceiver}
]

{ #category : 'helpers' }
InstructionStreamSteppingTest >> send: aString super: aBoolean numArgs: anInteger [

	operations add: { #send:super:numArgs:. aString. aBoolean. anInteger }
]

{ #category : 'running' }
InstructionStreamSteppingTest >> setUp [

	super setUp.
	operations := OrderedCollection new
]

{ #category : 'test-interpreter' }
InstructionStreamSteppingTest >> testBlockWithASuperSendHasCorrectNumberOfArguments [

	| aMethod aCompiledBlock aStream |
	aMethod := self classUnderTest >> #methodWithASuperBlock.
	aCompiledBlock := aMethod literals at:1.

	self assert: aCompiledBlock isCompiledBlock.

	aStream := InstructionStream on: aCompiledBlock.

	aStream interpretWithClient: self.

	self assert: operations fifth equals: { #directSuperSend:numArgs:. #aSuperMethod:with:. 2 }
]

{ #category : 'test-interpreter' }
InstructionStreamSteppingTest >> testBlockWithASuperWithoutArgumentsSendHasCorrectNumberOfArguments [

	| aMethod aCompiledBlock aStream |
	aMethod := self classUnderTest >> #methodWithASuperBlockWithoutArguments.
	aCompiledBlock := aMethod literals at:1.

	self assert: aCompiledBlock isCompiledBlock.

	aStream := InstructionStream on: aCompiledBlock.

	aStream interpretWithClient: self.

	self assert: operations third equals: { #directSuperSend:numArgs:. #yourself. 0 }
]

{ #category : 'tests-step through' }
InstructionStreamSteppingTest >> testSimulatedTerminationOfProcessDoNotCorruptTheContext [

	| initialContext aContext receiver semaphore process suspendedProcess return errorThatHappen |

	receiver := self classUnderTest new.
	semaphore := Semaphore new.
	process := [ semaphore wait ] forkAt: Processor activePriority + 1.
	receiver process: process.
	suspendedProcess := [ receiver callingAMethodSuspendedBeforeTheTerminateOfAnotherProcess ] forkAt: Processor activePriority + 1.

	initialContext := aContext := suspendedProcess suspendedContext.
	[ initialContext method = (self classUnderTest >> #callingAMethodSuspendedBeforeTheTerminateOfAnotherProcess) ]
		whileFalse: [ initialContext := initialContext sender].

	[return := suspendedProcess stepToHome: initialContext]
		on: Exception do: [:e | errorThatHappen := e ].

	self assert: errorThatHappen isNil.
	self assert: receiver expectedValue equals: 42
]

{ #category : 'tests-step through' }
InstructionStreamSteppingTest >> testSimulatingAMethodWithHaltHasCorrectContext [

	| initialContext aContext receiver suspendedProcess return |

	receiver := self classUnderTest new.
	suspendedProcess := [ receiver callingAMethodWithHalt ] forkAt: Processor activePriority + 1.

	initialContext := aContext := suspendedProcess suspendedContext.
	[ initialContext method = (self classUnderTest >> #callingAMethodWithHalt) ]
		whileFalse: [ initialContext := initialContext sender].

	return := suspendedProcess stepToHome: initialContext.

	"Suspended process should stop in the exception"
	self assert: return method equals: (Object >> #halt).
	self assert: return size equals: 1
]

{ #category : 'tests-step through' }
InstructionStreamSteppingTest >> testStepThroughInAMethodWithMNU [

	| initialContext aContext receiver suspendedProcess suspendedContext lastMarked |

	receiver := self classUnderTest new.
	suspendedProcess := [ receiver callingAMethodWithMNU ] forkAt: Processor activePriority + 1.

	initialContext := aContext := suspendedProcess suspendedContext.
	[ initialContext method = (self classUnderTest >> #callingAMethodWithMNU) ]
		whileFalse: [ initialContext := initialContext sender].

	suspendedContext := suspendedProcess stepToHome: initialContext.

	[suspendedContext method hasPragmaNamed: #debuggerCompleteToSender]
		whileTrue: [ 
			lastMarked := suspendedContext.
			suspendedContext := suspendedContext sender ].

	"Suspended process should stop in the exception"
	self assert: lastMarked method equals: (self classUnderTest lookupSelector: #doesNotUnderstand:)
]

{ #category : 'tests-step through' }
InstructionStreamSteppingTest >> testStepThroughInAMethodWithoutError [

	| initialContext aContext receiver suspendedProcess return |

	receiver := self classUnderTest new.
	suspendedProcess := [ receiver callingAMethodWithSuspendAndReturnANumber ] forkAt: Processor activePriority + 1.

	initialContext := aContext := suspendedProcess suspendedContext.
	[ initialContext method = (self classUnderTest >> #callingAMethodWithSuspendAndReturnANumber) ]
		whileFalse: [ initialContext := initialContext sender].

	return := suspendedProcess stepToHome: initialContext.

	"Suspended process should stop in the exception"
	self assert: return method equals: (self classUnderTest >> #callingAMethodWithSuspendAndReturnANumber).
	self assert: return top equals: 42
]
