"
I am an SUnit Test of Context.

See pages 430-437 of A. Goldberg and D. Robson's  Smalltalk-80 The Language (aka the purple book), which deal with Contexts. My fixtures are from their example. (The Squeak byte codes are not quite the same as Smalltalk-80.)
My fixtures are:
aReceiver         - just some arbitrary object, ""Rectangle origin: 100@100 corner: 200@200""
aSender           - just some arbitrary object, thisContext
aCompiledMethod - just some arbitrary method, ""Rectangle rightCenter"".
aMethodContext   - just some arbitray context ...  
nonActiveBlockContext  - the Context of a block whose home context is not in its sender chain (because the method that created this block already returned)

"
Class {
	#name : 'ContextTest',
	#superclass : 'TestCase',
	#instVars : [
		'aCompiledMethod',
		'aReceiver',
		'aMethodContext',
		'aSender',
		'nonActiveBlockContext',
		'anArgument'
	],
	#classVars : [
		'Block'
	],
	#category : 'Kernel-Tests-Methods',
	#package : 'Kernel-Tests',
	#tag : 'Methods'
}

{ #category : 'helper' }
ContextTest class >> createBlock [
	"we reference self to force a full block"
	^ [ self . thisContext ]
]

{ #category : 'helper' }
ContextTest class >> returnNonActiveContextOfBlock [
	Block := self createBlock.
	^ Block value
]

{ #category : 'running' }
ContextTest >> setUp [

	super setUp.
	aCompiledMethod := Rectangle compiledMethodAt: #areasOutside:.
	aReceiver := 100 @ 100 corner: 200 @ 200.
	aSender := thisContext.
	anArgument := 420 @ 420 corner: 200 @ 200.
	aMethodContext := Context
		                  sender: aSender
		                  receiver: aReceiver
		                  method: aCompiledMethod
		                  arguments: { anArgument }
]

{ #category : 'running' }
ContextTest >> tearDown [

	aMethodContext compiledCode stepIntoQuickMethods: false.
	super tearDown
]

{ #category : 'tests' }
ContextTest >> testActiveHome [
	| block |
	"Active BlockContext Home And ActiveHome"
	block := [ self. "to make it non-clean" thisContext home == thisContext activeHome].
	self assert: block class equals: FullBlockClosure.
	self assert: block value.

	"for Method Context"
	self
		assert: aMethodContext activeHome identicalTo: aMethodContext;
		assert: aMethodContext activeHome identicalTo: aMethodContext home.

	"non-active block context"
	nonActiveBlockContext := self class returnNonActiveContextOfBlock.
	self assert: (nonActiveBlockContext activeHome) isNil
]

{ #category : 'tests' }
ContextTest >> testBlockCannotReturn [

	| p |
	p := [ thisContext pc: nil ] newProcess.
	[
	p suspendedContext method selector = #pc: and: [
		p suspendedContext sender isDead ] ] whileFalse: [ p step ].
	self assert: p suspendedContext method selector equals: #pc:.
	self assert: p suspendedContext sender isDead.
	self assert: p suspendedContext pc equals: p suspendedContext method endPC.

	p step. "step into a return to a dead sender"

	self assert: p suspendedContext method selector equals: #cannotReturn:
]

{ #category : 'tests' }
ContextTest >> testCannotReturn [
	| context p |
	p := [context := thisContext] fork.
	[p isTerminated] whileFalse: [ (Delay forMilliseconds: 10) wait ].

	[ context resume: #result.
	self assert: false description: 'ContextCannotReturn should be signaled' ]
		 on: ContextCannotReturn do: [ :err |
			self assert: err result equals: #result.
			self assert: err target equals: context]
]

{ #category : 'tests' }
ContextTest >> testFindContextSuchThat [
	self assert: (aMethodContext findContextSuchThat: [ :each | true ]) printString equals: aMethodContext printString.
	self assert: (aMethodContext hasContext: aMethodContext)
]

{ #category : 'tests' }
ContextTest >> testHasNonLocalReturn [
	"the method has a non-local return, in some block inside"
	self assert: thisContext method hasNonLocalReturn.
	"but the context that executes not (as it is later another block context"
	self deny: thisContext hasNonLocalReturn.
	"this block context has no non-local return"
	[ self deny: thisContext hasNonLocalReturn] value.
	"again, this is about the context locally, nested blocks to not count"
	[ self deny: thisContext hasNonLocalReturn. [ ^ self ] ] value.
	[ self deny: thisContext hasNonLocalReturn ] value.
	"We need to have the ^ self but as this assertion is at the end is not a problem if we return"
	[ self assert: thisContext hasNonLocalReturn. ^ self ] value.
]

{ #category : 'tests' }
ContextTest >> testHome [

	self assert: aMethodContext home identicalTo: aMethodContext.
	[ self assert: thisContext home identicalTo:  thisContext sender] value
]

{ #category : 'tests' }
ContextTest >> testHomeMethod [

	self assert: aMethodContext homeMethod identicalTo: aMethodContext home method.
	[ self assert: thisContext homeMethod identicalTo:  thisContext home method] value
]

{ #category : 'tests' }
ContextTest >> testJump [
	#(exampleClosure exampleSend exampleStore) do: [ :selector |
		self verifyJumpWithSelector: selector ]
]

{ #category : 'tests' }
ContextTest >> testMethodContext [

	self assert: aMethodContext home isNotNil.
	self assert: aMethodContext receiver isNotNil.
	self assert: (aMethodContext method isKindOf: CompiledMethod)
]

{ #category : 'tests' }
ContextTest >> testMethodContextPrintDetails [

	self
		assert: (String streamContents: [ :stream |
				 aMethodContext printDetails: stream ])
		equals: 'Rectangle>>areasOutside:
	Receiver: (100@100) corner: (200@200)
	Receiver''s instance variables: 
		origin: 	(100@100)
		corner: 	(200@200)

	Arguments and temporary variables: 
		temp1: 	(200@200) corner: (420@420)
'
]

{ #category : 'tests' }
ContextTest >> testNoStepIntoQuickMethod [

	|newContext |
	aMethodContext := Context
		sender: thisContext
		receiver: aReceiver
		method: (Rectangle >>#center)
		arguments: #().

	"We step into #center to get on the topLeft message.
	We did not change context."
	aMethodContext step.

	"Now we step into #topLeft which is a quick method.
	The primitive should be executed as the flag to step into quick methods
	has not been set."
	newContext := aMethodContext step.
	self assert: newContext identicalTo: aMethodContext
]

{ #category : 'tests' }
ContextTest >> testNonActiveBlockContextHome [
	nonActiveBlockContext := self class returnNonActiveContextOfBlock.
	self assert: (nonActiveBlockContext home) isNotNil
]

{ #category : 'tests' }
ContextTest >> testReturn [
	"Why am I overriding setUp? Because sender must be thisContext, i.e, testReturn, not setUp."

	aMethodContext := Context
		sender: thisContext
		receiver: aReceiver
		method: aCompiledMethod
		arguments: #().
	self assert: (aMethodContext return: 5) equals: 5
]

{ #category : 'tests' }
ContextTest >> testSetUp [
	"Note: In addition to verifying that the setUp worked the way it was expected to, testSetUp is used to illustrate the meaning of the simple access methods, methods that are not normally otherwise 'tested'"

	self deny: aMethodContext isDead.
	self assert: aMethodContext home equals: aMethodContext.
	self assert: aMethodContext receiver equals: aReceiver.
	self assert: (aMethodContext method isKindOf: CompiledMethod).
	self assert: aMethodContext method equals: aCompiledMethod.
	self assert: aMethodContext client printString equals: 'ContextTest>>#testSetUp'
]

{ #category : 'tests' }
ContextTest >> testStepIntoQuickMethod [

	|newContext quickMethod|
	aMethodContext := Context
		sender: thisContext
		receiver: aReceiver
		method: (Rectangle >>#center)
		arguments: #().

	"We step into #center to get on the topLeft message.
	We did not change context."
	aMethodContext step.

	"Now we step into #topLeft which is a quick method"
	aMethodContext compiledCode stepIntoQuickMethods: true.
	newContext := aMethodContext step.
	quickMethod := (Rectangle >> #topLeft).
	self assert: newContext sender identicalTo: aMethodContext.
	self assert: newContext method identicalTo: quickMethod.
	self assert: quickMethod isQuick.

	"The quick method contains 3 bytecodes:
	- primitive 264: a 3 bytecode primitive (initialPC)
	- a push of a value (endPC - 1)
	- the return top instruction (endPC)
	When activating the quic method, we arrive on the PC corresponding to the primitive.
	This one is not suposed to be stepped.
	The first steppable bytecode is the push just before the return.
	We do that by setting the PC to aMethod initialPC + 3 (the number of bytes of the primitive)."
	self assert: newContext pc equals: (quickMethod initialPC + 3)
]

{ #category : 'tests' }
ContextTest >> testStepIntoQuickMethodBoolean [

	self deny: aMethodContext compiledCode stepIntoQuickMethods
]

{ #category : 'tests' }
ContextTest >> testStepIntoQuickMethodInCompiledBlock [

	|newContext|
	newContext := [ ] asContext.
	newContext compiledCode stepIntoQuickMethods: false.
	self deny: newContext method stepIntoQuickMethods.
	newContext compiledCode stepIntoQuickMethods: true.
	self assert: newContext method stepIntoQuickMethods
]

{ #category : 'tests' }
ContextTest >> testStepIntoReturnSelfMethod [

	|newContext quickMethod|
	aMethodContext := Context
		sender: thisContext
		receiver: SimulationMock new
		method: (SimulationMock >>#exampleSelfReturnCall)
		arguments: #().

	"(SimulationMock >>#exampleSelfReturnCall) just executes
	self yourself
	We step over self to get on the yourself message.
	We did not change context."
	aMethodContext step.

	"Now we step into #yourself which is a quick method just returning self"
	aMethodContext compiledCode stepIntoQuickMethods: true.
	newContext := aMethodContext step.
	quickMethod := (Object >> #yourself).
	self assert: newContext sender identicalTo: aMethodContext.
	self assert: newContext method identicalTo: quickMethod.
	self assert: quickMethod isQuick.

	"The quick method contains 3 bytecodes:
	- primitive 256: a 3 bytecode primitive (initialPC)
	- the return self instruction (endPC)
	When activating the quick method, we arrive on the PC corresponding to the primitive.
	This one is not suposed to be stepped.
	The first steppable bytecode is the push just before the return.
	We do that by setting the PC to aMethod initialPC + 3 (the number of bytes of the primitive)."
	self assert: newContext pc equals: (quickMethod initialPC + 3)
]

{ #category : 'tests' }
ContextTest >> verifyJumpWithSelector: selector [
	| guineaPig normalStackp readOnlyStackp |
	guineaPig := SimulationMock new.
	normalStackp := (guineaPig perform: selector) stackPtr.
	guineaPig beReadOnlyObject.
	[ readOnlyStackp := (guineaPig perform: selector) stackPtr ]
		on: ModificationForbidden
		do: [ :ex | ex resumeUnchecked: nil ].
	self assert: normalStackp equals: readOnlyStackp
]
