"
I represent the Alt key modifier. Look at my superclass for a more detailed explanaition.
"
Class {
	#name : #KMAltModifier,
	#superclass : #KMModifier,
	#category : 'Keymapping-KeyCombinations'
}

{ #category : #accessing }
KMAltModifier >> eventCode [
	^32
]

{ #category : #initialization }
KMAltModifier >> initialize [
	super initialize.
	identifier := #a.
	name := 'Alt'.
]

{ #category : #matching }
KMAltModifier >> matchesEvent: aKeyboardEvent [
	^ aKeyboardEvent altKeyPressed
]
