Class {
	#name : 'XmlReporterTest',
	#superclass : 'AbstractReporterTest',
	#category : 'TestRunner-Tests-Reporters',
	#package : 'TestRunner-Tests',
	#tag : 'Reporters'
}

{ #category : 'tests' }
XmlReporterTest class >> exampleOfTestFailureException [

	^ TestFailure new messageText: 'Assertion failed'
]

{ #category : 'running' }
XmlReporterTest >> setUp [

	super setUp.
	reporter := XmlReporter new
]

{ #category : 'tests' }
XmlReporterTest >> testBuildingAttributesForFailureTag [

	| aDictionnary aFailure aTestCase |
	aTestCase := GreenTest suite tests at: 1.
	aFailure := self class exampleOfTestFailureException.
	aDictionnary := reporter
		                buildAttributesForFailure: aFailure
		                duringTest: aTestCase.
	self
		assert: aDictionnary size equals: 2;
		assert: (aDictionnary at: 'type') equals: TestFailure;
		assert: (aDictionnary at: 'message') equals: 'Assertion failed'
]

{ #category : 'tests' }
XmlReporterTest >> testBuildingAttributesForTestCaseTag [

	| aDictionnary aTestCase |
	aTestCase := GreenTest suite tests at: 1.
	aDictionnary := reporter buildAttributesForTestCase: aTestCase.
	self
		assert: aDictionnary size equals: 3;
		assert: (aDictionnary at: 'classname') equals: 'TestRunner.Tests.Utilities.Test.GreenTest';
		assert: (aDictionnary at: 'name') equals: 'testPass';
		assert: (aDictionnary at: 'time') equals: nil
]
