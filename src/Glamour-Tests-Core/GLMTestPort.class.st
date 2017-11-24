Class {
	#name : #GLMTestPort,
	#superclass : #GLMPanePort,
	#instVars : [
		'seenTransmissions'
	],
	#category : #'Glamour-Tests-Core'
}

{ #category : #accessing }
GLMTestPort >> receive: aTransmission [ 
	 
	self seenTransmissions addLast: aTransmission. 
	^super receive: aTransmission
]

{ #category : #accessing }
GLMTestPort >> receive: aTransmission in: aContext [
	self seenTransmissions addLast: aTransmission. 
	^super receive: aTransmission in: aContext
]

{ #category : #accessing }
GLMTestPort >> seenTransmissions [ 
	 
	^seenTransmissions ifNil: [seenTransmissions := OrderedCollection new]
]
