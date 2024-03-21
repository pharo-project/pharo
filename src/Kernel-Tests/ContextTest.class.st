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
		'instVarForTestLookupSymbol',
		'nonActiveBlockContext',
		'anArgument'
	],
	#classVars : [
		'Block',
		'classVarForTestLookupSymbol'
	],
	#category : 'Kernel-Tests-Methods',
	#package : 'Kernel-Tests',
	#tag : 'Methods'
}

{ #category : 'closures for testing' }
ContextTest class >> contextWithTempForTesting [
	| string |
	string := 'test'.
	^ [self class. string asUppercase] asContext
]

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

{ #category : 'private' }
ContextTest >> privRestartArgBlockTest [
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"

	| firstTimeThrough |
	firstTimeThrough := true.

	self
		assert: 30
		equals:
			([ :a |
			| b |
			self assert: 10 equals: a.
			self assert: nil identicalTo: b.
			b := a + 20.
			firstTimeThrough
				ifTrue: [ firstTimeThrough := false.
					thisContext restart ].
			b ] value: 10)
]

{ #category : 'private' }
ContextTest >> privRestartBlockArgsNoRemoteTempsTest [
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"

	self
		assert: 30
		equals:
			([ :a :first |
			| b |
			self assert: 10 equals: a.
			self assert: nil identicalTo: b.
			b := a + 20.
			first
				ifTrue: [ "Cheat and modify one of the args so we will not loop endlessly"
					thisContext tempAt: 2 put: false.
					thisContext restart ].
			b ] value: 10 value: true)
]

{ #category : 'private' }
ContextTest >> privRestartBlockTest [
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"

	| a firstTimeThrough |
	firstTimeThrough := true.
	a := 10.

	self
		assert: 30
		equals:
			[ | b |
			self assert: 10 equals: a.
			self assert: nil identicalTo: b.
			b := a + 20.
			firstTimeThrough
				ifTrue: [ firstTimeThrough := false.
					thisContext restart ].
			b ] value
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
	aMethodContext stepIntoQuickMethod: false.
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
ContextTest >> testAstScope [

	| scope |
	"test that we get a scope for optimized blocks"
	aMethodContext pc: 88 + 1.
	scope := aMethodContext astScope.
	self assert: scope isOptimized.

	"test that we do not get a block scope when pushing a block on the stack"
	aMethodContext pc: 95 + 1.
	scope := aMethodContext astScope.
	self assert: aMethodContext sourceNodeExecuted isBlock.
	self deny: scope isBlockScope
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
	Arguments and temporary variables: 
		aRectangle: 	(200@200) corner: (420@420)
	Receiver''s instance variables: 
		origin: 	(100@100)
		corner: 	(200@200)

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
	aMethodContext stepIntoQuickMethod: true.
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

	self deny: aMethodContext stepIntoQuickMethod
]

{ #category : 'tests' }
ContextTest >> testStepIntoQuickMethodInCompiledBlock [

	|newContext|
	newContext := [ ] asContext.
	newContext stepIntoQuickMethod: false.
	self deny: newContext method stepIntoQuickMethods.
	newContext stepIntoQuickMethod: true.
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
	aMethodContext stepIntoQuickMethod: true.
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
ContextTest >> testSteppingAQuickMethod [

	|newContext|
	aMethodContext := Context
		sender: thisContext
		receiver: aReceiver
		method: (Rectangle >>#center)
		arguments: #().

	aMethodContext step.
	aMethodContext stepIntoQuickMethod: true.
	newContext := aMethodContext step.

	"We're in the quick method now, it should be steppable"
	self assert: newContext sender identicalTo: aMethodContext.
	self deny: newContext instructionStream willReturn.

	newContext := newContext step.
	self assert: newContext instructionStream willReturn.

	newContext := newContext step.
	self assert: newContext identicalTo: aMethodContext
]

{ #category : 'tests' }
ContextTest >> testSteppingReturnSelfMethod [

	|newContext|
	aMethodContext := Context
		sender: thisContext
		receiver: SimulationMock new
		method: (SimulationMock >>#exampleSelfReturnCall)
		arguments: #().

	aMethodContext step.
	aMethodContext stepIntoQuickMethod: true.
	newContext := aMethodContext step.

	"We're in the quick method now, it should be steppable"
	self assert: newContext sender identicalTo: aMethodContext.
	self assert: newContext instructionStream willReturn.

	newContext := newContext step.
	self assert: newContext identicalTo: aMethodContext
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
