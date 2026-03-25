"
I am a configuration for the tests runner plugin.

I can be converted as a test suite via #asTestSuite.
"
Class {
	#name : 'DTTestsRunnerConfiguration',
	#superclass : 'DTPluginConfiguration',
	#category : 'DrTests-TestsRunner-Base',
	#package : 'DrTests-TestsRunner',
	#tag : 'Base'
}

{ #category : 'converting' }
DTTestsRunnerConfiguration >> asTestSuite [

	| suite |
	suite := TestSuite new.
	"drTestsIsAbstract -> This method allows to either test a test case 
	 instance or a test suite depending on kind of item."
	self items reject: #drTestsIsAbstract thenDo: [ :aTestCase | 
		"drTestsSuite -> This method allows to either add a test case 
		 instance or a test suite depending on kind of item."
		suite addTests: aTestCase drTestsSuite tests ].
	^ suite
]
