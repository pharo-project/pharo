Class {
	#name : #NewCommandLineRunnerTest,
	#superclass : #TestCase,
	#instVars : [
		'runner'
	],
	#category : #'TestRunner-Tests-Core'
}

{ #category : #running }
NewCommandLineRunnerTest >> setUp [

	super setUp.
	runner := NewCommandLineRunner new
]

{ #category : #tests }
NewCommandLineRunnerTest >> testRunnerHasDefaultReporter [

	runner hasDefaultReporter
]
