Class {
	#name : #StdReporterTest,
	#superclass : #AbstractReporterTest,
	#category : #'TestRunner-Tests-Reporters'
}

{ #category : #tests }
StdReporterTest >> testCurrentTestIsIncrementAfterEachTestCaseExecution [

	| runner |
	runner := NewCommandLineRunner new reporter: reporter.
	runner runCase: (GreenTest suite tests at: 1).
	self assert: reporter currentTest equals: 2.
	runner runCase: (GreenTest suite tests at: 1).
	self assert: reporter currentTest equals: 3
]

{ #category : #tests }
StdReporterTest >> testCurrentTestIsOneInitialy [

	self assert: reporter currentTest equals: 1
]
