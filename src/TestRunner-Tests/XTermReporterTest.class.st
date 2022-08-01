Class {
	#name : #XTermReporterTest,
	#superclass : #AbstractReporterTest,
	#category : #'TestRunner-Tests-Reporters'
}

{ #category : #tests }
XTermReporterTest >> testOutputDriver [

	self 
	 assert: reporter defaultOutputDriver
	 equals: Stdio
]
