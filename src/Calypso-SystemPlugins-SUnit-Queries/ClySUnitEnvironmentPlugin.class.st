"
I extend items with information about contained tests.

I decorate package, classes and methods with ClyTestResultProperty. Browser plugin uses it to show special icon/button and install test related commands.

Also I provide failed tests group to show all such tests together.

When I am activated on environment I subscribe to test history announcer. And when tests run I ask environment to process results (I wrap SUnit event with my ClyTestCaseRan).
"
Class {
	#name : #ClySUnitEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #controlling }
ClySUnitEnvironmentPlugin >> attachToSystem [

	TestCase historyAnnouncer 
		weak when: TestSuiteEnded send: #testRan: to: self
]

{ #category : #'query metadata' }
ClySUnitEnvironmentPlugin >> collectMetadataOfMethods: aQueryResult [
	| property |
	property := 	ClyTestResultProperty new.
		
	aQueryResult rawItemsOf: ClyMethod do: [ :method | 
		(method isTestMethod and: [ method methodClass isAbstract not ]) ifTrue: [ 
			property accumulateTestMethod: method]].
	
	property allCount > 0 ifTrue: [ aQueryResult addMetaProperty: property ]
]

{ #category : #'method groups' }
ClySUnitEnvironmentPlugin >> collectMethodGroupProviders [ 

	^{ClyFailedTestGroupProvider new. ClyExpectedFailureMethodGroupProvider new}
]

{ #category : #'item decoration' }
ClySUnitEnvironmentPlugin >> decorateBrowserItem: anItem ofClass: aClass [

	(aClass isTestCase not or: [aClass isAbstract]) ifTrue: [ ^self ].
	
	anItem addProperty: (ClyTestResultProperty ofTestCase: aClass)
]

{ #category : #'item decoration' }
ClySUnitEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	(self isTestMethod: aMethod) ifFalse: [ ^self ].
	
	anItem addProperty: (ClyTestResultProperty ofMethod: aMethod)
]

{ #category : #'item decoration' }
ClySUnitEnvironmentPlugin >> decorateBrowserItem: anItem ofMethodGroup: aMethodGroup [
]

{ #category : #'item decoration' }
ClySUnitEnvironmentPlugin >> decorateBrowserItem: anItem ofPackage: aPackage [
	"this can be slow and proper implementation will require recomputing for every test run
	which slow down running of big test suite. 
	So for package and class group we only detect that it has tests"

	(aPackage classes anySatisfy: [:testCase | 
		testCase isTestCase and: [testCase isAbstract not]])
		ifFalse: [ ^self ].
		
	anItem addProperty: ClyTestResultProperty new
]

{ #category : #controlling }
ClySUnitEnvironmentPlugin >> detachFromSystem [

	TestCase historyAnnouncer unsubscribe: self
]

{ #category : #'item decoration' }
ClySUnitEnvironmentPlugin >> isTestClass: aClass [
	
	^aClass isTestCase or: [ 
		"Following logic is to detect tests from traits but without explicit dependency. 
		So we think that traits are test classes if they are used in test cases"
		aClass users anySatisfy: [ :eachUser | self isTestClass: eachUser ] ]
]

{ #category : #'item decoration' }
ClySUnitEnvironmentPlugin >> isTestMethod: aMethod [
	
	aMethod numArgs isZero ifFalse: [ ^ false ].
	"unary selectors starting with 'should' are supposed to be treated as test methods too"
	((aMethod selector beginsWith: 'test') or: [ aMethod selector beginsWith: 'should' ])
		ifFalse: [ ^ false ].
	
	^self isTestClass: aMethod methodClass
]

{ #category : #controlling }
ClySUnitEnvironmentPlugin >> testRan: aTestSuiteEnded [
	| event |
	
	"testResult actually returns the TestCase class ran"	
	event := ClyTestCaseRan testCase: aTestSuiteEnded testResult.
	
	environment systemChanged: event
]
