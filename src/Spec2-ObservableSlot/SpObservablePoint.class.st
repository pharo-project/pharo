"
I am a point used to test observable properties.
I have an observable property x and a non-observable property y.

I use TObservable that has methods to ease the usage of my observable properties.
"
Class {
	#name : #SpObservablePoint,
	#superclass : #Object,
	#traits : 'SpTObservable',
	#classTraits : 'SpTObservable classTrait',
	#instVars : [
		'#x => SpObservableSlot',
		'#y'
	],
	#category : #'Spec2-ObservableSlot'
}

{ #category : #initialization }
SpObservablePoint >> initialize [
	super initialize.
	self class initializeSlots: self.
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
