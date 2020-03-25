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
	#name : #ContextTest,
	#superclass : #TestCase,
	#instVars : [
		'aCompiledMethod',
		'aReceiver',
		'aMethodContext',
		'aSender',
		'instVarForTestLookupSymbol',
		'nonActiveBlockContext'
	],
	#classVars : [
		'Block',
		'classVarForTestLookupSymbol'
	],
	#category : #'Kernel-Tests-Methods'
}

{ #category : #'closures for testing' }
ContextTest class >> contextWithTempForTesting [ 
	| string |
	string := 'test'.
	^ [self class. string asUppercase] asContext.

]

{ #category : #helper }
ContextTest class >> createBlock [
	^ [ thisContext ].
]

{ #category : #helper }
ContextTest class >> returnNonActiveContextOfBlock [
	Block := self createBlock.
	^ Block value
]

{ #category : #private }
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

{ #category : #private }
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

{ #category : #private }
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

{ #category : #running }
ContextTest >> setUp [
	super setUp.
	aCompiledMethod := Rectangle methodDict at: #rightCenter.
	aReceiver := 100@100 corner: 200@200.
	aSender := thisContext.
	aMethodContext := Context sender: aSender receiver: aReceiver method: aCompiledMethod arguments: #(). 
]

{ #category : #tests }
ContextTest >> testActivateReturnValue [
	self assert: ((aSender activateReturn: aMethodContext value: #()) isKindOf: Context).
	self assert: (aSender activateReturn: aMethodContext value: #()) receiver equals: aMethodContext
]

{ #category : #tests }
ContextTest >> testActiveBlockContextHomeAndActiveHome [
	[ self assert: thisContext home identicalTo:  thisContext activeHome.] value
]

{ #category : #private }
ContextTest >> testActiveHomeMethodContext [
	self
		assert: aMethodContext activeHome identicalTo: aMethodContext;
		assert: aMethodContext activeHome identicalTo: aMethodContext home
]

{ #category : #tests }
ContextTest >> testFindContextSuchThat [
	self assert: (aMethodContext findContextSuchThat: [ :each | true ]) printString equals: aMethodContext printString.
	self assert: (aMethodContext hasContext: aMethodContext)
]

{ #category : #tests }
ContextTest >> testMethodContext [

	self assert: aMethodContext home notNil.
	self assert: aMethodContext receiver notNil.
	self assert: (aMethodContext method isKindOf: CompiledMethod).
]

{ #category : #tests }
ContextTest >> testMethodIsBottomContext [
	self
		assert: aMethodContext bottomContext equals: aSender;
		assert: aMethodContext secondFromBottom equals: aMethodContext
]

{ #category : #tests }
ContextTest >> testNonActiveBlockContextActiveHome [
	nonActiveBlockContext := self class returnNonActiveContextOfBlock.
	self assert: (nonActiveBlockContext activeHome) isNil.
]

{ #category : #tests }
ContextTest >> testNonActiveBlockContextHome [
	nonActiveBlockContext := self class returnNonActiveContextOfBlock.
	self assert: (nonActiveBlockContext home) isNotNil.
]

{ #category : #tests }
ContextTest >> testReturn [
	"Why am I overriding setUp? Because sender must be thisContext, i.e, testReturn, not setUp."

	aMethodContext := Context
		sender: thisContext
		receiver: aReceiver
		method: aCompiledMethod
		arguments: #().
	self assert: (aMethodContext return: 5) equals: 5
]

{ #category : #tests }
ContextTest >> testSetUp [
	"Note: In addition to verifying that the setUp worked the way it was expected to, testSetUp is used to illustrate the meaning of the simple access methods, methods that are not normally otherwise 'tested'"

	self deny: aMethodContext isDead.
	self assert: aMethodContext home equals: aMethodContext.
	self assert: aMethodContext receiver equals: aReceiver.
	self assert: (aMethodContext method isKindOf: CompiledMethod).
	self assert: aMethodContext method equals: aCompiledMethod.
	self assert: aMethodContext client printString equals: 'ContextTest>>#testSetUp'
]

{ #category : #tests }
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
