"
I represent the Ctrl key modifier. Look at my superclass for a more detailed explanaition.
"
Class {
	#name : #KMCtrlModifier,
	#superclass : #KMModifier,
	#category : #'Keymapping-KeyCombinations'
}

{ #category : #accessing }
KMCtrlModifier >> eventCode [
	^16
]

{ #category : #initialization }
KMCtrlModifier >> initialize [
	super initialize.
	identifier := #c.
	name := 'Ctrl'.
]

{ #category : #matching }
KMCtrlModifier >> matchesEvent: aKeyboardEvent [
	^ aKeyboardEvent controlKeyPressed
]
