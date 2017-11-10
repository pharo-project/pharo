"
Athens text background 
"
Class {
	#name : #AthensTextBackground,
	#superclass : #TextAttribute,
	#instVars : [
		'color'
	],
	#category : #Athens-Text
}

{ #category : #accessing }
AthensTextBackground >> color [

	^ color
]

{ #category : #accessing }
AthensTextBackground >> color: anObject [

	color := anObject
]

{ #category : #scanning }
AthensTextBackground >> dominates: other [
	^ other class == self class
]

{ #category : #scanning }
AthensTextBackground >> emphasizeScanner: scanner [
	"Set the emphasis for text display"
	scanner textBackground: color
]

{ #category : #comparing }
AthensTextBackground >> hash [
	^ color hash
]

{ #category : #printing }
AthensTextBackground >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' code: '; print: color
]
