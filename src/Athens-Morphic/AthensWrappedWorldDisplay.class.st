"
An AthensWrappedWorldDisplay acts as a DisplayScreen
for a wrapped WorldMorph.

Instance Variables
	realDisplay:		the realdisplay
	wrapMorph:	the wrapped world

"
Class {
	#name : #AthensWrappedWorldDisplay,
	#superclass : #Object,
	#instVars : [
		'realDisplay',
		'wrapMorph'
	],
	#category : #'Athens-Morphic'
}

{ #category : #'instance creation' }
AthensWrappedWorldDisplay class >> on: aWorldWrapMorph [
	^ self new wrapMorph:aWorldWrapMorph
]

{ #category : #'display protocol' }
AthensWrappedWorldDisplay >> boundingBox [
	^ wrapMorph ifNotNil: [ wrapMorph fullBounds ] ifNil: [ 0 @ 0 extent: 0 @ 0 ]
]

{ #category : #'display protocol' }
AthensWrappedWorldDisplay >> doesNotUnderstand: aMessage [
	"delegate all not implemented methods to the real display"

	^ (realDisplay respondsTo: aMessage selector)
		ifTrue: [ realDisplay perform: aMessage selector withEnoughArguments: aMessage arguments ]
		ifFalse: [ super doesNotUnderstand: aMessage ]
]

{ #category : #'display protocol' }
AthensWrappedWorldDisplay >> extent [
	^ wrapMorph ifNotNil: [ wrapMorph extent ] ifNil: [ 0 @ 0 ]
]

{ #category : #initialization }
AthensWrappedWorldDisplay >> initialize [
	super initialize.

	realDisplay := Display
]

{ #category : #accessing }
AthensWrappedWorldDisplay >> wrapMorph [
	^ wrapMorph
]

{ #category : #accessing }
AthensWrappedWorldDisplay >> wrapMorph: anObject [
	wrapMorph := anObject
]
