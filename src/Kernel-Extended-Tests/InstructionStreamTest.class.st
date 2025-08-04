Class {
	#name : 'InstructionStreamTest',
	#superclass : 'TestCase',
	#instVars : [
		'operations'
	],
	#category : 'Kernel-Extended-Tests-Methods',
	#package : 'Kernel-Extended-Tests',
	#tag : 'Methods'
}

{ #category : 'helpers' }
InstructionStreamTest >> blockReturnTop [

		operations add: { #blockReturnTop }
]

{ #category : 'helpers' }
InstructionStreamTest >> classUnderTest [

	^ ClassUsedInInstructionStreamTest
]

{ #category : 'helpers' }
InstructionStreamTest >> directedSuperSend: aString numArgs: anInteger [

	operations add: { #directSuperSend:numArgs:. aString. anInteger }
]

{ #category : 'accessing' }
InstructionStreamTest >> pc: anInteger [

	"Ignore the PC"
]

{ #category : 'helpers' }
InstructionStreamTest >> pushConstant: anInteger [

	operations add: { #pushConstant. anInteger }
]

{ #category : 'helpers' }
InstructionStreamTest >> pushLiteralVariable: aLiteralVariable [

	operations add: { #pushConstant. aLiteralVariable }
]

{ #category : 'helpers' }
InstructionStreamTest >> pushReceiver [

	operations add: {#pushReceiver}
]

{ #category : 'helpers' }
InstructionStreamTest >> send: aString super: aBoolean numArgs: anInteger [

	operations add: { #send:super:numArgs:. aString. aBoolean. anInteger }
]

{ #category : 'running' }
InstructionStreamTest >> setUp [

	super setUp.
	operations := OrderedCollection new
]

{ #category : 'tests' }
InstructionStreamTest >> testBlockWithASuperSendHasCorrectNumberOfArguments [

	| aMethod aCompiledBlock aStream |
	aMethod := self classUnderTest >> #methodWithASuperBlock.
	aCompiledBlock := aMethod literals at:1.

	self assert: aCompiledBlock isCompiledBlock.

	aStream := InstructionStream on: aCompiledBlock.

	aStream interpretWithClient: self.

	self assert: operations fifth equals: { #directSuperSend:numArgs:. #aSuperMethod:with:. 2 }
]

{ #category : 'tests' }
InstructionStreamTest >> testBlockWithASuperWithoutArgumentsSendHasCorrectNumberOfArguments [

	| aMethod aCompiledBlock aStream |
	aMethod := self classUnderTest >> #methodWithASuperBlockWithoutArguments.
	aCompiledBlock := aMethod literals at:1.

	self assert: aCompiledBlock isCompiledBlock.

	aStream := InstructionStream on: aCompiledBlock.

	aStream interpretWithClient: self.

	self assert: operations third equals: { #directSuperSend:numArgs:. #yourself. 0 }
]

{ #category : 'tests' }
InstructionStreamTest >> testNextPcAnswerNextBytecodePcInCaseOfExtension [

	| stream methodToTest startingPc |
	methodToTest := ClassUsedInInstructionStreamTest
	                >> #newMethodWithALotOfLiteralsToTestExtensions.

	startingPc := methodToTest ast firstPcForNode:
		              (methodToTest ast allChildren select: [ :e |
			               e isMessage ]) anyOne.
	stream := InstructionStream new method: methodToTest pc: startingPc.

	"16rE0 16rE1 are extensions that help handling lot of literals.
	When such an extension is encoutered the next pc shoudl skip them.
	In our case we have 1 extension and the bytecode so the next pc is at the size of this extension plus the size of the bytecode extended (2 + 2)"

	self assert: (stream nextPc: 16rE0) equals: startingPc + 4
]

{ #category : 'tests - example' }
InstructionStreamTest >> testSimulatedTerminationOfProcessDoNotCorruptTheContext [

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

{ #category : 'tests - example' }
InstructionStreamTest >> testSimulatingAMethodWithHaltHasCorrectContext [

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

{ #category : 'tests - example' }
InstructionStreamTest >> testStepContextNonTakenConditionalJumpBytecodes [

	| context |
	"Assume ifTrue: conditional jumps are compiled as jumpIfFalse"
	context := [ (true ifTrue: [ 1 ] ifFalse: [2]) + 7 ] asContext.

	6 timesRepeat: [
		"push true, not jump, push 1, jump to end, push 7, send +"
		context step ].

	self assert: context top equals: 8
]

{ #category : 'tests - example' }
InstructionStreamTest >> testStepContextTakenConditionalJumpBytecodes [

	| context |
	"Assume ifTrue: conditional jumps are compiled as jumpIfFalse"
	context := [ false ifTrue: [ 1 ]. 2 ] asContext.

	5 timesRepeat: [
		"push false, jump, push nil, pop, push 2"
		context step ].

	self assert: context top equals: 2
]

{ #category : 'tests - example' }
InstructionStreamTest >> testStepNonTakenConditionalJumpBytecodes [

	| context stream |
	"Assume ifTrue: conditional jumps are compiled as jumpIfFalse"
	context := [ (true ifTrue: [ 1 ] ifFalse: [2]) + 7 ] asContext.
	stream := InstructionStream on: context method.

	6 timesRepeat: [
		"push true, not jump, push 1, jump to end, push 7, send +"
		stream interpretNextInstructionFor: context ].

	self assert: context top equals: 8
]

{ #category : 'tests - example' }
InstructionStreamTest >> testStepSendPopsArgumentsAndPushesResultBytecodes [

	| context stream |
	context := [ 1 + 2 ] asContext.
	stream := InstructionStream on: context method.

	3 timesRepeat: [
		stream interpretNextInstructionFor: context ].

	self assert: context stackPtr equals: 1 "elements on the stack".
	self assert: context top equals: 3
]

{ #category : 'tests - example' }
InstructionStreamTest >> testStepSingleBytecode [

	| context stream |
	context := [ 1 ] asContext.
	stream := InstructionStream on: context method.

	stream interpretNextInstructionFor: context.

	self assert: context top equals: 1
]

{ #category : 'tests - example' }
InstructionStreamTest >> testStepTakenConditionalJumpBytecodes [

	| context stream |
	"Assume ifTrue: conditional jumps are compiled as jumpIfFalse"
	context := [ false ifTrue: [ 1 ]. 2 ] asContext.
	stream := InstructionStream on: context method.

	5 timesRepeat: [
		"push false, jump, push nil, pop, push 2"
		stream interpretNextInstructionFor: context ].

	self assert: context top equals: 2
]

{ #category : 'tests - example' }
InstructionStreamTest >> testStepThroughInAMethodWithMNU [

	| initialContext aContext receiver suspendedProcess return |

	receiver := self classUnderTest new.
	suspendedProcess := [ receiver callingAMethodWithMNU ] forkAt: Processor activePriority + 1.

	initialContext := aContext := suspendedProcess suspendedContext.
	[ initialContext method = (self classUnderTest >> #callingAMethodWithMNU) ]
		whileFalse: [ initialContext := initialContext sender].

	return := suspendedProcess stepToHome: initialContext.

	"Suspended process should stop in the exception"
	self assert: return method equals: (self classUnderTest lookupSelector: #doesNotUnderstand:)
]

{ #category : 'tests - example' }
InstructionStreamTest >> testStepThroughInAMethodWithoutError [

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

{ #category : 'tests - example' }
InstructionStreamTest >> testStepTwoBytecodes [

	| context stream |
	context := [ 1 + 2 ] asContext.
	stream := InstructionStream on: context method.

	2 timesRepeat: [
		stream interpretNextInstructionFor: context ].

	self assert: context top equals: 2
]

{ #category : 'tests - example' }
InstructionStreamTest >> testSteppingSendsDirectSend [

	| initialContext aContext receiver |

	receiver := self classUnderTest new.

	initialContext := Context
		sender: nil receiver: receiver
		method: self classUnderTest >> #valueOfBlockWithSupersend
		arguments: #().

	aContext := initialContext.

	[aContext willFailReturn]
		whileFalse: [aContext := aContext step].

	self assert: receiver expectedValue equals: 42
]
