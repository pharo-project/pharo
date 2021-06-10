"
A DrTestsUITest is a test class for testing the behavior of DrTests-TestRunner UI
"
Class {
	#name : #DrTestsTestRunnerUITest,
	#superclass : #TestCase,
	#instVars : [
		'drTest'
	],
	#category : #'DrTests-Tests'
}

{ #category : #running }
DrTestsTestRunnerUITest >> setUp [

	super setUp.
	drTest := DrTests on: { DTTestsRunnerPlugin }.
]

{ #category : #tests }
DrTestsTestRunnerUITest >> testRunTestsUpdatesUIWithResults [

	drTest pluginPresenter startButton performAction.
	self assert: drTest pluginPresenter resultsList roots notEmpty
]
