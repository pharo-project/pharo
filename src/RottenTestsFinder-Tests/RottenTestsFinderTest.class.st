"
A RottenTestsFinderTest is a test class for testing the behavior of RottenTestsFinder
"
Class {
	#name : 'RottenTestsFinderTest',
	#superclass : 'RTFMetaTest',
	#category : 'RottenTestsFinder-Tests-Core',
	#package : 'RottenTestsFinder-Tests',
	#tag : 'Core'
}

{ #category : 'tests-analyzing' }
RottenTestsFinderTest >> testAnalyze [
	| rottenTestsFound |
	rottenTestsFound := RottenTestsFinder analyze: RTFFakeTestClassTest suite.

	self
		assert: rottenTestsFound rottenTests size equals: 3;
		assert: (rottenTestsFound rottenCompiledMethods anySatisfy: [ :m |
			m methodClass = RTFFakeTestSuperClassTest
				and: [ m selector = #superNotAssertHelper ] ]);
		assert: (rottenTestsFound rottenCompiledMethods anySatisfy: [ :m |
			m methodClass = RTFFakeTestClassTest
				and: [ m selector = #rottenHelper ] ]);
		assert: (rottenTestsFound rottenCompiledMethods anySatisfy: [ :m |
			m methodClass = RTFFakeTestClassTest
				and: [ m selector = #testWithoutAssertionRun ] ])
]
