Class {
	#name : #TimelineTest,
	#superclass : #TestCase,
	#instVars : [
		'timeline'
	],
	#category : #'Spec-Core-Widgets-Athens'
}

{ #category : #running }
TimelineTest >> setUp [
	super setUp.
	timeline := Timeline new
]

{ #category : #running }
TimelineTest >> testFFGoesToNegativeInfinity [
	timeline fastForward .
	self assert: timeline ticks equals: Float negativeInfinity 
]

{ #category : #running }
TimelineTest >> testRewindGoesToPositiveInfinity [
	timeline rewind.
	self assert: timeline ticks equals: Float infinity
]

{ #category : #running }
TimelineTest >> testTickIncrementsOne [
	timeline tick.
	self assert: timeline ticks equals: 1
]

{ #category : #running }
TimelineTest >> testTickIncrementsStepBy [
	timeline stepBy: 10.
	timeline tick.
	self assert: timeline ticks equals: 10
]
