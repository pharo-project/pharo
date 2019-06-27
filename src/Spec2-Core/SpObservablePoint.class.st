"
I am a point used to test observable properties.
I have an observable property x and a non-observable property y.

I use TObservable that has methods to ease the usage of my observable properties.
"
Class {
	#name : #SpObservablePoint,
	#superclass : #Object,
	#traits : 'TSpObservable',
	#classTraits : 'TSpObservable classTrait',
	#instVars : [
		'#x => SpObservableSlot',
		'#y'
	],
	#category : #'Spec2-Core-Observable'
}

{ #category : #initialization }
SpObservablePoint >> initialize [
	super initialize.
	self class initializeSlots: self.
]

{ #category : #events }
SpObservablePoint >> observablePropertyNamed: aName [
	| slot |
	
	slot := self class slotNamed: aName.
	slot isObservableSlot
		ifFalse: [ SpNonObservableSlotError signal: aName ].

	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	^ slot rawRead: self
]

{ #category : #accessing }
SpObservablePoint >> x [
	^ x
]

{ #category : #accessing }
SpObservablePoint >> x: anInteger [ 
	x := anInteger
]

{ #category : #accessing }
SpObservablePoint >> y [
	^ y
]

{ #category : #accessing }
SpObservablePoint >> y: anInteger [ 
	y := anInteger
]
