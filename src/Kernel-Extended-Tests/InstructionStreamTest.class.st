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
InstructionStreamTest >> testNextPcAnswerNextBytecodePcInCaseOfExtension [

	| stream methodToTest startingPc |
	methodToTest := ClassUsedInInstructionStreamTest
	                >> #newMethodWithALotOfLiteralsToTestExtensions.

	startingPc := methodToTest ast firstPcForNode:
		              (methodToTest ast withAllChildren select: [ :e |
			               e isMessage ]) anyOne.
	stream := InstructionStream new method: methodToTest pc: startingPc.

	"16rE0 16rE1 are extensions that help handling lot of literals.
	When such an extension is encoutered the next pc shoudl skip them.
	In our case we have 1 extension and the bytecode so the next pc is at the size of this extension plus the size of the bytecode extended (2 + 2)"

	self assert: (stream nextPc: 16rE0) equals: startingPc + 4
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
