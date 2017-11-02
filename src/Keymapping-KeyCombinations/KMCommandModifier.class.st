"
I represent the default modifier key for the current platform, late bound to Meta or Ctrl key modifier depending on the platform.
"
Class {
	#name : #KMCommandModifier,
	#superclass : #KMModifier,
	#category : 'Keymapping-KeyCombinations'
}

{ #category : #accessing }
KMCommandModifier >> eventCode [
	^ 64
]

{ #category : #initialization }
KMCommandModifier >> initialize [
	super initialize.
	identifier := #k.
	name := 'Cmd'
]

{ #category : #matching }
KMCommandModifier >> matchesEvent: aKeyboardEvent [
	^  aKeyboardEvent commandKeyPressed
]
