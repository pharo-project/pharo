"
I do not match with any keyboard event.

I respond to the Null object pattern, since I represent the inexistance of a key combination.
"
Class {
	#name : #KMNoShortcut,
	#superclass : #KMKeyCombination,
	#category : #'Keymapping-KeyCombinations'
}

{ #category : #combining }
KMNoShortcut >> + aKMModifier [
	^ aKMModifier
]

{ #category : #combining }
KMNoShortcut >> , aShortcut [
	^ aShortcut
]

{ #category : #matching }
KMNoShortcut >> matches: anEventBuffer [
	^ false
]

{ #category : #matching }
KMNoShortcut >> matchesCompletely: anEventBuffer [
	^ false
]

{ #category : #printing }
KMNoShortcut >> printOn: aStream [
]
