"
I am an abstract class which specify the behaviour for reporting the result of tests execution 
"
Class {
	#name : #AbstractReporter,
	#superclass : #Object,
	#instVars : [
		'suiteExecutionDuration',
		'currentCaseDuration'
	],
	#category : #'TestRunner-Core-Reporter'
}

{ #category : #testing }
AbstractReporter class >> isAbstract [

	^ self = AbstractReporter
]
