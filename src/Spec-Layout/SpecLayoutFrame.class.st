"
I am a wrapper for the layout of widget.

Each framework can then adapt me as it wants in its adapters
"
Class {
	#name : #SpecLayoutFrame,
	#superclass : #Object,
	#instVars : [
		'bottomFraction',
		'bottomOffset',
		'leftFraction',
		'leftOffset',
		'rightFraction',
		'rightOffset',
		'topFraction',
		'topOffset',
		'owner'
	],
	#category : #'Spec-Layout-Support'
}

{ #category : #'instance creation' }
SpecLayoutFrame class >> identity [
	"by default a layout frame is initialized to represent the identity transformation"
	^ self new
]

{ #category : #'instance creation' }
SpecLayoutFrame class >> owner: anObject [

	^ self new owner: anObject; yourself.
]

{ #category : #converting }
SpecLayoutFrame >> asArray [
	 
	^ { leftFraction . 
		topFraction .
		rightFraction .
		bottomFraction .	
		leftOffset .
		topOffset .
		rightOffset .
		bottomOffset } 
]

{ #category : #converting }
SpecLayoutFrame >> asSpecLayoutFrame [

	^ self
]

{ #category : #accessing }
SpecLayoutFrame >> bottomFraction [
	
	^ bottomFraction
]

{ #category : #accessing }
SpecLayoutFrame >> bottomFraction: anObject [
	
	bottomFraction := anObject
]

{ #category : #accessing }
SpecLayoutFrame >> bottomOffset [
	
	^ bottomOffset
]

{ #category : #accessing }
SpecLayoutFrame >> bottomOffset: anObject [
	
	bottomOffset := anObject
]

{ #category : #accessing }
SpecLayoutFrame >> bottomRightOffset: aPoint [

	bottomOffset := aPoint y.
	rightOffset := aPoint x.
]

{ #category : #generation }
SpecLayoutFrame >> generateSpec [

	^ self asArray generateSpec
]

{ #category : #testing }
SpecLayoutFrame >> hasNoOffsets [

	^ leftOffset = 0 and: [rightOffset = 0  and: [ topOffset = 0 and: [ bottomOffset = 0 ]]].
	
]

{ #category : #initialization }
SpecLayoutFrame >> initialize [

	super initialize.

	bottomFraction := 1.
	bottomOffset := 0.
	leftFraction := 0.
	leftOffset := 0.
	rightFraction := 1.
	rightOffset := 0.
	topFraction := 0.
	topOffset := 0.
]

{ #category : #accessing }
SpecLayoutFrame >> leftFraction [
	
	^ leftFraction
]

{ #category : #accessing }
SpecLayoutFrame >> leftFraction: anObject [
	
	leftFraction := anObject
]

{ #category : #accessing }
SpecLayoutFrame >> leftOffset [
	
	^ leftOffset
]

{ #category : #accessing }
SpecLayoutFrame >> leftOffset: anObject [
	
	leftOffset := anObject
]

{ #category : #accessing }
SpecLayoutFrame >> owner [

	"Every object instantiated by the SpecInerpreter should have an owner that will assign it to a particular presenter"
	^ owner
]

{ #category : #accessing }
SpecLayoutFrame >> owner: anObject [
	owner := anObject
]

{ #category : #accessing }
SpecLayoutFrame >> rightFraction [
	
	^ rightFraction
]

{ #category : #accessing }
SpecLayoutFrame >> rightFraction: anObject [
	
	rightFraction := anObject
]

{ #category : #accessing }
SpecLayoutFrame >> rightOffset [
	
	^ rightOffset
]

{ #category : #accessing }
SpecLayoutFrame >> rightOffset: anObject [
	
	rightOffset := anObject
]

{ #category : #accessing }
SpecLayoutFrame >> topFraction [
	
	^ topFraction
]

{ #category : #accessing }
SpecLayoutFrame >> topFraction: anObject [
	
	topFraction := anObject
]

{ #category : #accessing }
SpecLayoutFrame >> topLeftOffset: aPoint [

	topOffset := aPoint y.
	leftOffset := aPoint x.
]

{ #category : #accessing }
SpecLayoutFrame >> topOffset [
	
	^ topOffset
]

{ #category : #accessing }
SpecLayoutFrame >> topOffset: anObject [
	
	topOffset := anObject
]
