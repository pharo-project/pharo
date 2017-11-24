"
I am a debugging session for the SUnit debugger. 
I extract from the execution stack various data needed by the user interface of the SUnit debugger, like the test object and method.

Public API and Key Messages

- process:context: does the initialization of the session
- data is provided using the methods in the accessing protocol

    Instance Variables
	actualResult:		<Object>
	assertionContext:		<Object>
	expectedResult:		<Object>
	testObject:		<Object>
"
Class {
	#name : #GTSUnitDebugSession,
	#superclass : #DebugSession,
	#instVars : [
		'assertionContext',
		'expectedResult',
		'actualResult',
		'testContext',
		'assertionCallerContext'
	],
	#category : #'GT-SUnitDebugger'
}

{ #category : #accessing }
GTSUnitDebugSession class >> activationPredicate [

	^  GTSUnitActivationPredicate new
]

{ #category : #accessing }
GTSUnitDebugSession >> actualResult [
	
	^ actualResult
]

{ #category : #accessing }
GTSUnitDebugSession >> assertionCallerContext [

	^ assertionCallerContext
]

{ #category : #accessing }
GTSUnitDebugSession >> assertionContext [

	^ assertionContext
]

{ #category : #accessing }
GTSUnitDebugSession >> expectedResult [
	
	^ expectedResult 
]

{ #category : #initialization }
GTSUnitDebugSession >> initializeAssestionContextFrom: aContext [
	| frameworkTestClasses |
	
	frameworkTestClasses := {TestCase. TestCase superclass}.
	
	assertionContext := aContext findContextSuchThat: [ :anotherContext |
		anotherContext = testContext 
			ifTrue: [ 
				"the assestion context should be between the top context and before the test context"
				^ self  ].
		anotherContext sender notNil and: [ 
			 (frameworkTestClasses anySatisfy: [ :aTestClass |
				 aTestClass includesSelector: anotherContext sender method selector]) not ] ].
	assertionContext 
		ifNotNil: [ assertionCallerContext := assertionContext sender]
	
		
]

{ #category : #initialization }
GTSUnitDebugSession >> initializeTestContextFrom: aContext [

	"we first try to find a test method in the stack"
	testContext := aContext findContextSuchThat: [ :anotherContext |
		anotherContext method isTestMethod ].
	
	testContext ifNil: [
		"if we do not find a test method, perhaps we are in the context of a different method, such as setUp, so we are looking for the last occurence of the test case instance in the stack"
		testContext := aContext findContextSuchThat: [ :anotherContext |
			anotherContext receiver class isTestCase ] ].
	
	testContext ifNil: [ 
		"if we did not find anything, we fallback to the default context"
		testContext := aContext ]
]

{ #category : #testing }
GTSUnitDebugSession >> isActive [

	^ self isActive: self testContext
]

{ #category : #testing }
GTSUnitDebugSession >> isActive: aContext [

	^ aContext 
		ifNil: [ false ] 
		ifNotNil: [ 
			aContext isDead not and: [
				aContext ~= self process suspendedContext or: [aContext willReturn not ] ] ]
]

{ #category : #testing }
GTSUnitDebugSession >> overridesDefaultSetOrTearMethods [
	
	^ self overridesDefaultTearDownMethod or: [ 
		self overridesDefaultSetUpMethod ]
]

{ #category : #testing }
GTSUnitDebugSession >> overridesDefaultSetUpMethod [
	
	^ self setUpMethod methodClass ~= TestCase
]

{ #category : #testing }
GTSUnitDebugSession >> overridesDefaultTearDownMethod [
	
	^ self tearDownMethod methodClass ~= TestCase
]

{ #category : #initialization }
GTSUnitDebugSession >> process: aProcess context: aContext [

	super process: aProcess context: aContext.

	assertionContext ifNil: [
		self initializeTestContextFrom: aContext.
		self initializeAssestionContextFrom: aContext.
		self supportsDiff ifTrue: [ 
			actualResult := assertionContext tempAt: 1.
			expectedResult := assertionContext tempAt: 2 ]  ]
]

{ #category : #accessing }
GTSUnitDebugSession >> setUpMethod [

	^ self testObject class lookupSelector: #setUp
]

{ #category : #testing }
GTSUnitDebugSession >> supportsDiff [

	^ assertionContext 
		ifNil: [ false ]
		ifNotNil: [ assertionContext selector = #assert:equals: ]
]

{ #category : #accessing }
GTSUnitDebugSession >> tearDownMethod [

	^ self testObject class lookupSelector: #tearDown
]

{ #category : #accessing }
GTSUnitDebugSession >> testContext [

	^ testContext
]

{ #category : #accessing }
GTSUnitDebugSession >> testObject [

	^ testContext receiver
]
