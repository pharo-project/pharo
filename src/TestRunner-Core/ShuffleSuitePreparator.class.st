"
I am a suite preparator who randomise the order of tests in the giving testSuite.
"
Class {
	#name : 'ShuffleSuitePreparator',
	#superclass : 'AbstractSuitePreparator',
	#category : 'TestRunner-Core-SuitePreparators',
	#package : 'TestRunner-Core',
	#tag : 'SuitePreparators'
}

{ #category : 'applying' }
ShuffleSuitePreparator class >> applyOn: testSuite [
	"shuffle the suite. It could be useful for tests dependency detection"

	testSuite tests shuffle
]
