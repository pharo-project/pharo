"
I am a configuration for the tests runner plugin.

I can be converted as a test suite via #asTestSuite.
"
Class {
	#name : #DTTestsRunnerConfiguration,
	#superclass : #DTPluginConfiguration,
	#category : #'DrTests-TestsRunner'
}

{ #category : #converting }
DTTestsRunnerConfiguration >> asTestSuite [
	|newTestSuite|
	newTestSuite := TestSuite new.
	self items
		reject: #drTestsIsAbstract "This method allows to either test a test case instance or a test suite depending on kind of item."
		thenDo: [ :item | newTestSuite addTest: item drTestsSuite "This method allows to either add a test case instance or a test suite depending on kind of item." ].
	^ newTestSuite
]
