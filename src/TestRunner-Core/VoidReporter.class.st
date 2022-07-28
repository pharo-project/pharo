"
I am a reporter specialy use for testing the NewCommandLineRunner. Since using XmlReporter will create indesirable files or StdReporter which will display tests result in the terminal, it is usefull to use me to avoid such things
"
Class {
	#name : #VoidReporter,
	#superclass : #AbstractReporter,
	#category : #'TestRunner-Core-Reporters'
}

{ #category : #reporting }
VoidReporter >> reportError: aTestError of: aTestCase [

	
]

{ #category : #reporting }
VoidReporter >> reportFailure: aTestFailure of: aTestCase [

]

{ #category : #reporting }
VoidReporter >> reportPass: aTestCase [

	
]

{ #category : #reporting }
VoidReporter >> reportResult: aTestResult [

	
]

{ #category : #reporting }
VoidReporter >> reportSuite: aTestSuite runBlock: aBlock [

	aBlock value
]
