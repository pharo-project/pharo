"
I am responsible of finding rotten tests in a TestCase,

A rotten test is a test referencing an assertion method in its source code but does not call it at runtime.
"
Class {
	#name : 'RottenTestsFinder',
	#superclass : 'SUnitVisitor',
	#instVars : [
		'rottenTests',
		'visitableObject',
		'testsVisitedCount',
		'testsRunCount'
	],
	#category : 'RottenTestsFinder-Core',
	#package : 'RottenTestsFinder',
	#tag : 'Core'
}

{ #category : 'analyzing' }
RottenTestsFinder class >> analyze: aVisitableObject [
	^ self new
		visitableObject: aVisitableObject;
		analyze;
		rottenTests
]

{ #category : 'accessing' }
RottenTestsFinder class >> assertionCompiledMethods [
	"Returns the list of CompiledMethods used to assert things during tests."
	^ ((Object methodsInProtocol: 'asserting') , (TestAsserter methodsInProtocol: 'asserting')
			collect: [ :o |
				o class = RTFMethodTracer
					ifTrue: [ o method ]
					ifFalse: [ o ] ])
			reject: [ :compiledMethod |
				#(#fail: #fail #signalFailure: #skip #skip:) includes: compiledMethod selector ]
]

{ #category : 'accessing' }
RottenTestsFinder class >> assertionSelectors [
	"Returns the list of selectors corresponding to methods used to assert things during tests."
	^ self assertionCompiledMethods collect: #selector
]

{ #category : 'analyzing' }
RottenTestsFinder >> analyze [
	self runTests.

	self rottenTests
		testsVisitedCount: testsVisitedCount;
		testsRunCount: testsRunCount
]

{ #category : 'accessing' }
RottenTestsFinder >> assertionCompiledMethods [
	^ self class assertionCompiledMethods
]

{ #category : 'accessing' }
RottenTestsFinder >> assertionSelectors [
	^ self class assertionSelectors
]

{ #category : 'initialization' }
RottenTestsFinder >> initialize [
	super initialize.
	testsVisitedCount := 0.
	testsRunCount := 0
]

{ #category : 'accessing' }
RottenTestsFinder >> rottenTests [
	^ rottenTests
]

{ #category : 'accessing' }
RottenTestsFinder >> rottenTests: anObject [
	rottenTests := anObject
]

{ #category : 'private' }
RottenTestsFinder >> runTests [
	rottenTests := RottenTestsSet new.
	self visitableObject
		acceptSUnitVisitor: self
]

{ #category : 'accessing' }
RottenTestsFinder >> testsRunCount [
	^ testsRunCount
]

{ #category : 'accessing' }
RottenTestsFinder >> testsVisitedCount [
	^ testsVisitedCount
]

{ #category : 'visiting' }
RottenTestsFinder >> visitTestCase: aTestCase [
	| compiledMethod rootCallNode callSites |
	testsVisitedCount := testsVisitedCount + 1.
	compiledMethod := aTestCase class lowestCompiledMethodInInheritanceChainNamed: aTestCase selector.

	rootCallNode := (RTFSelfCallInterpreter new
							considerClassesThat: [ :class | class inheritsFrom: TestAsserter ];
							send: compiledMethod selector fromClass: aTestCase class;
							rootSelfCall) cleanSubTreesNotLeadingToAssertPrimitive.

	callSites := rootCallNode subCalls.

	rootCallNode installMetalinks.

	"Run the test, only interesting if green, else ignore."
	[
		|testResult|
		testResult := aTestCase run.
		(testResult hasPassed and: [ aTestCase isExpectedFailure not and: [ testResult skippedCount = 0 ] ])
			ifFalse: [ "If the test is not green, no need to continue analysis."
				RTFTestAnalysed signalTest: compiledMethod.
				^ self ].
	] ensure: [
		testsRunCount := testsRunCount + 1.
		rootCallNode uninstallMetalinks ].

	"Checks whether test is rotten or not (i.e. is there an assertion primitive or helper call site not executed)."
	(callSites allSatisfy: #hasBeenExecuted)
		ifFalse: [
			self rottenTests
				add: ((compiledMethod origin generateRottenTestFromCompiledMethod: compiledMethod testCase: aTestCase)
							callSites: (callSites reject: #hasBeenExecuted thenCollect: #astNode);
							yourself) ].

	"Checks whether there is rotten helpers or not."
	callSites
		select: [ :callSite | callSite isCallToAssertPrimitive not and: [ callSite hasBeenExecuted ] ]
		thenDo: [ :selfCallNode |
			|visitor|
			visitor := RTFRottenHelpersFinder new.
			selfCallNode acceptVisitor: visitor.
			self rottenTests addAll: visitor rottenHelpers ].


	RTFTestAnalysed signalTest: compiledMethod
]

{ #category : 'accessing' }
RottenTestsFinder >> visitableObject [
	^ visitableObject
]

{ #category : 'accessing' }
RottenTestsFinder >> visitableObject: anObject [
	visitableObject := anObject
]
