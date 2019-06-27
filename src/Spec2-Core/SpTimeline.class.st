Class {
	#name : #SpTimeline,
	#superclass : #Object,
	#traits : 'TSpObservable',
	#classTraits : 'TSpObservable classTrait',
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
	ticks := highestValue.
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

{ #category : #events }
SpTimeline >> observablePropertyNamed: aName [
	| slot |
	
	slot := self class slotNamed: aName.
	slot isObservableSlot
		ifFalse: [ SpNonObservableSlotError signal: aName ].

	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	^ slot rawRead: self
]

{ #category : #'tick management' }
SpTimeline >> rewind [ 
	ticks := lowestValue.
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
	highestValue := anAmount max: highestValue . 
	lowestValue := anAmount min: lowestValue.
]

{ #category : #initialization }
SpTimeline >> whenTickDo: aBlock [
	self property: #ticks whenChangedDo: aBlock
]
