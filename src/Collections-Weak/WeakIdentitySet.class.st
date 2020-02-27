"
I'm a IdentitySet with weak elements
"
Class {
	#name : #WeakIdentitySet,
	#superclass : #WeakSet,
	#category : #'Collections-Weak-Base'
}

{ #category : #private }
WeakIdentitySet >> scanFor: anObject [
	"Scan the key array for the first slot containing either flag (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or raise an error if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := anObject identityHash \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == flag or: [ element enclosedElement == anObject ])
			ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]

{ #category : #private }
WeakIdentitySet >> scanForEmptySlotFor: aKey [
	"Scan the key array for the first slot containing an empty slot (indicated by flag or a nil). Answer the index of that slot. This method will be overridden in various subclasses that have different interpretations for matching elements."
	
	| index start |
	index := start := aKey identityHash \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == flag or: [ element == nil ]) ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]
