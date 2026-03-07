"
I define a strategy for `NewTestRunner` to prepare a test suite before execution.

We would like to apply processes like randomisation or proxies to tests and suites, hence `SuitePreparator>>#applyOn:` is sent after building a TestSuite and before executing its tests.
"
Class {
	#name : 'SuitePreparator',
	#superclass : 'Object',
	#category : 'TestRunner-Core-SuitePreparators',
	#package : 'TestRunner-Core',
	#tag : 'SuitePreparators'
}

{ #category : 'applying' }
SuitePreparator >> applyOn: testSuite [
	
	testSuite tests do: [ :aTestCase | self applyOnCase: aTestCase ]
]

{ #category : 'applying' }
SuitePreparator >> applyOnCase: aTestCase [

	
]

{ #category : 'applying' }
SuitePreparator >> applyOnSuites: aCollection [
	"apply to every testSuite the specified process (see self applyOn:)"

	aCollection do: [ :suite | self applyOn: suite ]
]
