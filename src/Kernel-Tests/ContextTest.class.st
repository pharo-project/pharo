"
I am an SUnit Test of Context.

See pages 430-437 of A. Goldberg and D. Robson's  Smalltalk-80 The Language (aka the purple book), which deal with Contexts. My fixtures are from their example. (The Squeak byte codes are not quite the same as Smalltalk-80.)
My fixtures are:
aReceiver         - just some arbitrary object, ""Rectangle origin: 100@100 corner: 200@200""
aSender           - just some arbitrary object, thisContext
aCompiledMethod - just some arbitrary method, ""Rectangle rightCenter"".
aMethodContext   - just some arbitray context ...  


"
Class {
	#name : #ContextTest,
	#superclass : #TestCase,
	#instVars : [
		'aCompiledMethod',
		'aReceiver',
		'aMethodContext',
		'aSender',
		'instVarForTestLookupSymbol'
	],
	#classVars : [
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

{ #category : #private }
ContextTest >> privRestartArgBlockTest [
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"
	|firstTimeThrough |
	firstTimeThrough := true.
	
	self assert: 30 equals: ([:a | |b| 
		self assert: 10 = a .
		self assert: nil == b.
		b := a + 20. 
		firstTimeThrough ifTrue: [
			firstTimeThrough := false.
			thisContext restart.].
		b] value: 10)

]

{ #category : #private }
ContextTest >> privRestartBlockArgsNoRemoteTempsTest [
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"
	
	self assert: 30 equals: ([:a :first | |b| 
		self assert: 10 = a .
		self assert: nil == b.
		b := a + 20. 
		first ifTrue: [
			"Cheat and modify one of the args so we will not loop endlessly"
			 thisContext tempAt: 2 put: false.
			thisContext restart.].
		b] value: 10 value: true)

]

{ #category : #private }
ContextTest >> privRestartBlockTest [
	"This tests may loop endlessly if incorrect, so call it from another method testing it does not time out"
	|a firstTimeThrough |
	firstTimeThrough := true.
	a := 10.
	
	self assert: 30 equals: [|b| 
		self assert: 10 = a .
		self assert: nil == b.
		b := a + 20. 
		firstTimeThrough ifTrue: [
			firstTimeThrough := false.
			thisContext restart.].
		b] value

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
	self assert:  ((aSender activateReturn: aMethodContext value: #()) isKindOf: Context).
	self assert:  ((aSender activateReturn: aMethodContext value: #()) receiver = aMethodContext).
]

{ #category : #tests }
ContextTest >> testCannotReturn [
	| context p |
	p := [context := thisContext] fork.
	[p isTerminated] whileFalse: [ 10 milliSeconds wait ].

	[ context resume: #result.
	self assert: false description: 'ContextCannotReturn should be signaled' ]
		 on: ContextCannotReturn do: [ :err |
			self assert: err result equals: #result.
			self assert: err target equals: context]
]

{ #category : #tests }
ContextTest >> testClosureRestart [
	"Test that various combinations of closures are restarted with the expected values"
	"no args, no remote temps blocks are not tested, as I don't know how to do that programatically without ending up looping endlessly"
	self should: [self privRestartBlockTest] notTakeMoreThan: 0.1 second.
	self should: [self privRestartArgBlockTest] notTakeMoreThan: 0.1 second.
	"self should: [self privRestartBlockArgsNoRemoteTempsTest] notTakeMoreThan: 0.1 second" "FAILING!"
]

{ #category : #tests }
ContextTest >> testFindContextSuchThat [
	self assert: (aMethodContext findContextSuchThat: [:each| true]) printString = aMethodContext printString.
	self assert: (aMethodContext hasContext: aMethodContext). 
]

{ #category : #tests }
ContextTest >> testJump [
	#(exampleClosure exampleSend exampleStore) do: [ :selector |
		self verifyJumpWithSelector: selector ]
]

{ #category : #tests }
ContextTest >> testLookupSymbol [
	|localVar|
	localVar := 2.
	instVarForTestLookupSymbol := 3.
	classVarForTestLookupSymbol := 4.
	ContextTestAssistantA classVarForTestLookupSymbol: 5.
	"Check lookup of local variable"
	self assert: (thisContext lookupSymbol: #localVar) equals: 2.
	"Check lookup of instance variable"
	self assert: (thisContext lookupSymbol: #instVarForTestLookupSymbol) equals: 3.
	"Check lookup of class variable"
	self assert: (thisContext lookupSymbol: #classVarForTestLookupSymbol) equals: 4.
	"Check lookup of global variable"
	self assert: (thisContext lookupSymbol: #Smalltalk) equals: Smalltalk.
	"Check lookup of class variable declared by a superclass"
	self assert: (ContextTestAssistantB lookupSymbolInThisContext: #classVarForTestLookupSymbol) equals: 5.
	"Check lookup of non-existant variable"
	self assert: (thisContext lookupSymbol: #gibberishgibberish) equals: nil.
	
]

{ #category : #tests }
ContextTest >> testMethodContext [

	self assert: aMethodContext home notNil.
	self assert: aMethodContext receiver notNil.
	self assert: (aMethodContext method isKindOf: CompiledMethod).
]

{ #category : #tests }
ContextTest >> testMethodIsBottomContext [
	self assert: aMethodContext bottomContext = aSender.
	self assert: aMethodContext secondFromBottom = aMethodContext.
]

{ #category : #tests }
ContextTest >> testReturn [
	"Why am I overriding setUp? Because sender must be thisContext, i.e, testReturn, not setUp."
	aMethodContext := Context sender: thisContext receiver: aReceiver method: aCompiledMethod arguments: #(). 
	self assert: (aMethodContext return: 5) = 5.
]

{ #category : #tests }
ContextTest >> testSetUp [
	"Note: In addition to verifying that the setUp worked the way it was expected to, testSetUp is used to illustrate the meaning of the simple access methods, methods that are not normally otherwise 'tested'"
	
	self deny: aMethodContext isDead.
	self assert: aMethodContext home = aMethodContext.
	self assert: aMethodContext receiver = aReceiver.
	self assert: (aMethodContext method isKindOf: CompiledMethod).
	self assert: aMethodContext method = aCompiledMethod. 
	self assert: aMethodContext client printString = 'ContextTest>>#testSetUp'.

]

{ #category : #'tests - ast mapping' }
ContextTest >> testSourceNodeExecuted [
	| sourceNode |
	
	sourceNode := thisContext sender sender sourceNodeExecuted.
	self assert: sourceNode selector = #performTest.
]

{ #category : #'tests - ast mapping' }
ContextTest >> testSourceNodeExecutedWhenContextIsJustAtStartpc [
	| sourceNode context |
	
	context := Context sender: thisContext receiver: self method: self class >> testSelector arguments: #().
	sourceNode := context sourceNodeExecuted.
	self assert: sourceNode equals: (self class >> testSelector) ast sendNodes first receiver
]

{ #category : #tests }
ContextTest >> verifyJumpWithSelector: selector [
	| guineaPig normalStackp readOnlyStackp |
	guineaPig := SimulationMock new.
	normalStackp := (guineaPig perform: selector) stackPtr.
	guineaPig beReadOnlyObject.
	[ readOnlyStackp := (guineaPig perform: selector) stackPtr ]
		on: ModificationForbidden 
		do: [ :ex | ex resume ].
	self assert: normalStackp = readOnlyStackp.
	
]
