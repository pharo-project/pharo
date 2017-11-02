"
I represent the absense of a key modifier. I fulfill the role of a null pattern, having as default no printing and not matching.
"
Class {
	#name : #KMNoModifier,
	#superclass : #KMModifier,
	#category : 'Keymapping-KeyCombinations'
}

{ #category : #initialization }
KMNoModifier >> initialize [
	super initialize.
	identifier := #''.
	name := ''.
]
