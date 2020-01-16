Class {
	#name : #SpTimeline,
	#superclass : #Object,
	#traits : 'SpTObservable',
	#classTraits : 'SpTObservable classTrait',
	#instVars : [
		'#ticks => SpObservableSlot',
		'#highestValue',
		'#lowestValue',
		'#stepBy'
	],
	#category : #'Spec2-Core-Widgets-Athens'
}

{ #category : #'tick management' }
SpTimeline >> fastForward [

	ticks := highestValue
]

{ #category : #initialization }
SpTimeline >> initialize [

	self class initializeSlots: self.
	super initialize.
	ticks := 0.
	highestValue := Float negativeInfinity.
	lowestValue := Float infinity.
	stepBy := 1
]

{ #category : #'tick management' }
SpTimeline >> rewind [ 

	ticks := lowestValue
]

{ #category : #initialization }
SpTimeline >> stepBy: aNumber [

	stepBy := aNumber
]

{ #category : #'tick management' }
SpTimeline >> tick [
	self ticks: ticks + stepBy
]

{ #category : #accessing }
SpTimeline >> ticks [

	^ ticks
]

{ #category : #accessing }
SpTimeline >> ticks: anAmount [ 

	ticks := anAmount.
	highestValue := anAmount max: highestValue.
	lowestValue := anAmount min: lowestValue
]

{ #category : #initialization }
SpTimeline >> whenTickDo: aBlock [

	self property: #ticks whenChangedDo: aBlock
]
