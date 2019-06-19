Class {
	#name : #Timeline,
	#superclass : #Object,
	#traits : 'TSpecObservable',
	#classTraits : 'TSpecObservable classTrait',
	#instVars : [
		'#ticks => SpecObservableSlot',
		'#highestValue',
		'#lowestValue',
		'#stepBy'
	],
	#category : #'Spec-Core-Widgets-Athens'
}

{ #category : #'tick management' }
Timeline >> fastForward [
	ticks := highestValue.
]

{ #category : #initialization }
Timeline >> initialize [
	self class initializeSlots: self.
	super initialize.
	ticks := 0.
	highestValue := Float negativeInfinity.
	lowestValue := Float infinity.
	stepBy := 1
]

{ #category : #'tick management' }
Timeline >> rewind [ 
	ticks := lowestValue.
]

{ #category : #initialization }
Timeline >> stepBy: aNumber [
	stepBy := aNumber
]

{ #category : #'tick management' }
Timeline >> tick [
	self ticks: ticks + stepBy
]

{ #category : #accessing }
Timeline >> ticks [
	^ ticks
]

{ #category : #accessing }
Timeline >> ticks: anAmount [ 
	ticks := anAmount.
	highestValue := anAmount max: highestValue . 
	lowestValue := anAmount min: lowestValue.
]

{ #category : #initialization }
Timeline >> whenTickDo: aBlock [
	self property: #ticks whenChangedDo: aBlock
]
