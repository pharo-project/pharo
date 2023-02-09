Class {
	#name : #XTermReporterTest,
	#superclass : #AbstractReporterTest,
	#category : #'TestRunner-Tests-Reporters'
}

{ #category : #running }
XTermReporterTest >> setUp [

	super setUp.
	reporter := XTermReporter new
]

{ #category : #tests }
XTermReporterTest >> testOutputDriver [

	self 
	 assert: reporter defaultOutputDriver
	 equals: Stdio
]
