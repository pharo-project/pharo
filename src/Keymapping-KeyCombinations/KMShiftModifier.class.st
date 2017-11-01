"
I represent the Shift key modifier. Look at my superclass for a more detailed explanaition.
"
Class {
	#name : #KMShiftModifier,
	#superclass : #KMModifier,
	#category : 'Keymapping-KeyCombinations'
}

{ #category : #accessing }
KMShiftModifier >> eventCode [
	^8
]

{ #category : #initialization }
KMShiftModifier >> initialize [
	super initialize.
	identifier := #s.
	name := 'Shift'.
]

{ #category : #matching }
KMShiftModifier >> matchesEvent: aKeyboardEvent [
	^ aKeyboardEvent shiftPressed
]
