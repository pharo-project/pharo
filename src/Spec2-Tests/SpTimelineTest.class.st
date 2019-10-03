Class {
	#name : #SpTimelineTest,
	#superclass : #TestCase,
	#instVars : [
		'timeline'
	],
	#category : #'Spec2-Tests-Core'
}

{ #category : #running }
SpTimelineTest >> setUp [
	super setUp.
	timeline := SpTimeline new
]

{ #category : #running }
SpTimelineTest >> testFFGoesToNegativeInfinity [
	timeline fastForward .
	self assert: timeline ticks equals: Float negativeInfinity 
]

{ #category : #running }
SpTimelineTest >> testRewindGoesToPositiveInfinity [
	timeline rewind.
	self assert: timeline ticks equals: Float infinity
]

{ #category : #running }
SpTimelineTest >> testTickIncrementsOne [
	timeline tick.
	self assert: timeline ticks equals: 1
]

{ #category : #running }
SpTimelineTest >> testTickIncrementsStepBy [
	timeline stepBy: 10.
	timeline tick.
	self assert: timeline ticks equals: 10
]
