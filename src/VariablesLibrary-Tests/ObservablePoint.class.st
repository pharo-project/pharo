"
I am a mock point used to test observable properties. I have an observable property x and a non-observable property y.

I use TObservable that has methods to ease the usage of my observable properties.
"
Class {
	#name : #ObservablePoint,
	#superclass : #Object,
	#traits : 'TObservable',
	#classTraits : 'TObservable classTrait',
	#instVars : [
		'#x => ObservableSlot',
		'#y'
	],
	#category : #'VariablesLibrary-Tests-Observable'
}

{ #category : #initialization }
ObservablePoint >> initialize [
	super initialize.
	self class initializeSlots: self.
]

{ #category : #accessing }
ObservablePoint >> x [
	^ x
]

{ #category : #accessing }
ObservablePoint >> x: anInteger [ 
	x := anInteger
]

{ #category : #accessing }
ObservablePoint >> y [
	^ y
]

{ #category : #accessing }
ObservablePoint >> y: anInteger [ 
	y := anInteger
]
