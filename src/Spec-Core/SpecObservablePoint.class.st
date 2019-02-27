"
I am a point used to test observable properties.
I have an observable property x and a non-observable property y.

I use TObservable that has methods to ease the usage of my observable properties.
"
Class {
	#name : #SpecObservablePoint,
	#superclass : #Object,
	#traits : 'TSpecObservable',
	#classTraits : 'TSpecObservable classTrait',
	#instVars : [
		'#x => SpecObservableSlot',
		'#y'
	],
	#category : #'Spec-Core-Observable'
}

{ #category : #initialization }
SpecObservablePoint >> initialize [
	super initialize.
	self class initializeSlots: self.
]

{ #category : #events }
SpecObservablePoint >> observablePropertyNamed: aName [
	
	(self class slotNamed: aName) isObservableSlot
		ifFalse: [ SpecNonObservableSlotError signal: aName ].

	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	^ self instVarNamed: aName
]

{ #category : #accessing }
SpecObservablePoint >> x [
	^ x
]

{ #category : #accessing }
SpecObservablePoint >> x: anInteger [ 
	x := anInteger
]

{ #category : #accessing }
SpecObservablePoint >> y [
	^ y
]

{ #category : #accessing }
SpecObservablePoint >> y: anInteger [ 
	y := anInteger
]
