"
A RottenTestsFinderTest is a test class for testing the behavior of RottenTestsFinder
"
Class {
	#name : 'RottenTestsFinderTest',
	#superclass : 'TestCase',
	#category : 'RottenTestsFinder-Tests',
	#package : 'RottenTestsFinder-Tests'
}

{ #category : 'running' }
RottenTestsFinderTest >> setUp [
	"See RTFFakeTestSuperClass comment for more information."
	super setUp.
	RTFFakeTestSuperClass superclass: TestCase
]

{ #category : 'running' }
RottenTestsFinderTest >> tearDown [
	RTFFakeTestSuperClass superclass: Object.
	super tearDown
]

{ #category : 'tests-analyzing' }
RottenTestsFinderTest >> testAnalyze [
	| rottenTestsFound |
	rottenTestsFound := RottenTestsFinder analyze: RTFFakeTestClass suite.

	self
		assert: rottenTestsFound rottenTests size equals: 3;
		assert: (rottenTestsFound rottenCompiledMethods anySatisfy: [ :m |
			m methodClass = RTFFakeTestSuperClass
				and: [ m selector = #superNotAssertHelper ] ]);
		assert: (rottenTestsFound rottenCompiledMethods anySatisfy: [ :m |
			m methodClass = RTFFakeTestClass
				and: [ m selector = #rottenHelper ] ]);
		assert: (rottenTestsFound rottenCompiledMethods anySatisfy: [ :m |
			m methodClass = RTFFakeTestClass
				and: [ m selector = #testWithoutAssertionRun ] ])
]
