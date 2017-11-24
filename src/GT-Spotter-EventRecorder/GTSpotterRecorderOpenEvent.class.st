"
A GTSpotterRecorderOpenEvent  models the action of  opening Spotter.

Instance Variables
	timestamp:		<Object>

timestamp
	- xxxxx

"
Class {
	#name : #GTSpotterRecorderOpenEvent,
	#superclass : #GTSpotterRecorderEvent,
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderOpenEvent >> accept: aVisitor [
	^ aVisitor visitOpenEvent: self
]

{ #category : #testing }
GTSpotterRecorderOpenEvent >> initialize [
	super initialize.
	self initializeObfuscated.
]

{ #category : #testing }
GTSpotterRecorderOpenEvent >> isOpenSpotter [
	^ true
]
