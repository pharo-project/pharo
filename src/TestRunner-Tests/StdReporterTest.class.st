Class {
	#name : 'StdReporterTest',
	#superclass : 'AbstractReporterTest',
	#category : 'TestRunner-Tests-Reporters',
	#package : 'TestRunner-Tests',
	#tag : 'Reporters'
}

{ #category : 'running' }
StdReporterTest >> setUp [

	super setUp.
	reporter := StdReporter new
]

{ #category : 'tests' }
StdReporterTest >> testCurrentTestIsIncrementAfterEachTestCaseExecution [

	| runner |
	runner := NewCommandLineRunner new reporter: reporter.
	runner runCase: (GreenTest suite tests at: 1).
	self assert: reporter currentTest equals: 2.
	runner runCase: (GreenTest suite tests at: 1).
	self assert: reporter currentTest equals: 3
]

{ #category : 'tests' }
StdReporterTest >> testCurrentTestIsOneInitialy [

	self assert: reporter currentTest equals: 1
]
