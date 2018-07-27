"
I represent the Meta or Command key modifier. Look at my superclass for a more detailed explanaition.
"
Class {
	#name : #KMMetaModifier,
	#superclass : #KMModifier,
	#category : #'Keymapping-KeyCombinations'
}

{ #category : #comparing }
KMMetaModifier >> = aShortcut [

	^ aShortcut = OSPlatform current defaultModifier
]

{ #category : #accessing }
KMMetaModifier >> eventCode [

	^ OSPlatform current defaultModifier eventCode
]

{ #category : #comparing }
KMMetaModifier >> hash [
	"We should change the implementation of shortcuts to not use = for matching"
	| tmp |
	tmp :=  identifier hash.
	^ tmp
]

{ #category : #accessing }
KMMetaModifier >> identifier [

	^ OSPlatform current defaultModifier identifier
]

{ #category : #initialization }
KMMetaModifier >> initialize [
	super initialize.
	identifier := #m.
	name := 'Meta'.
]

{ #category : #matching }
KMMetaModifier >> matches: aKeyboardEvent [
	^ aKeyboardEvent asKeyCombination modifier = OSPlatform current defaultModifier
]

{ #category : #matching }
KMMetaModifier >> matchesEvent: aKeyboardEvent [
	^ OSPlatform current defaultModifier matchesEvent: aKeyboardEvent
]
