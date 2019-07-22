"
I am a wrapper for the layout of widget.

Each framework can then adapt me as it wants in its adapters
"
Class {
	#name : #SpLayoutFrame,
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
	#category : #'Spec2-Layout-Support'
}

{ #category : #'instance creation' }
SpLayoutFrame class >> identity [
	"by default a layout frame is initialized to represent the identity transformation"
	^ self new
]

{ #category : #'instance creation' }
SpLayoutFrame class >> owner: anObject [

	^ self new owner: anObject; yourself.
]

{ #category : #converting }
SpLayoutFrame >> asArray [
	 
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
SpLayoutFrame >> asSpLayoutFrame [

	^ self
]

{ #category : #accessing }
SpLayoutFrame >> bottomFraction [
	
	^ bottomFraction
]

{ #category : #accessing }
SpLayoutFrame >> bottomFraction: anObject [
	
	bottomFraction := anObject
]

{ #category : #accessing }
SpLayoutFrame >> bottomOffset [
	
	^ bottomOffset
]

{ #category : #accessing }
SpLayoutFrame >> bottomOffset: anObject [
	
	bottomOffset := anObject
]

{ #category : #accessing }
SpLayoutFrame >> bottomRightOffset: aPoint [

	bottomOffset := aPoint y.
	rightOffset := aPoint x.
]

{ #category : #testing }
SpLayoutFrame >> hasNoOffsets [

	^ leftOffset = 0 and: [rightOffset = 0  and: [ topOffset = 0 and: [ bottomOffset = 0 ]]].
	
]

{ #category : #initialization }
SpLayoutFrame >> initialize [

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
SpLayoutFrame >> leftFraction [
	
	^ leftFraction
]

{ #category : #accessing }
SpLayoutFrame >> leftFraction: anObject [
	
	leftFraction := anObject
]

{ #category : #accessing }
SpLayoutFrame >> leftOffset [
	
	^ leftOffset
]

{ #category : #accessing }
SpLayoutFrame >> leftOffset: anObject [
	
	leftOffset := anObject
]

{ #category : #accessing }
SpLayoutFrame >> owner [

	"Every object instantiated by the SpecInerpreter should have an owner that will assign it to a particular presenter"
	^ owner
]

{ #category : #accessing }
SpLayoutFrame >> owner: anObject [
	owner := anObject
]

{ #category : #accessing }
SpLayoutFrame >> rightFraction [
	
	^ rightFraction
]

{ #category : #accessing }
SpLayoutFrame >> rightFraction: anObject [
	
	rightFraction := anObject
]

{ #category : #accessing }
SpLayoutFrame >> rightOffset [
	
	^ rightOffset
]

{ #category : #accessing }
SpLayoutFrame >> rightOffset: anObject [
	
	rightOffset := anObject
]

{ #category : #accessing }
SpLayoutFrame >> topFraction [
	
	^ topFraction
]

{ #category : #accessing }
SpLayoutFrame >> topFraction: anObject [
	
	topFraction := anObject
]

{ #category : #accessing }
SpLayoutFrame >> topLeftOffset: aPoint [

	topOffset := aPoint y.
	leftOffset := aPoint x.
]

{ #category : #accessing }
SpLayoutFrame >> topOffset [
	
	^ topOffset
]

{ #category : #accessing }
SpLayoutFrame >> topOffset: anObject [
	
	topOffset := anObject
]
