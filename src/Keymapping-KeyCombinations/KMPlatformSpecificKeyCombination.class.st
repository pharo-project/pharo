"
I represent a platform specific key combination, one that should only be launched for a particular platform such as windows, unix or mac.

I'm a sort of decorator of a key combination.
"
Class {
	#name : #KMPlatformSpecificKeyCombination,
	#superclass : #Object,
	#instVars : [
		'shortcut',
		'platform'
	],
	#category : 'Keymapping-KeyCombinations'
}

{ #category : #'instance creation' }
KMPlatformSpecificKeyCombination class >> with: aShortcut onPlatform: aPlatform [
	^self new
			shortcut: aShortcut;
			platform: aPlatform;
			yourself
]

{ #category : #comparing }
KMPlatformSpecificKeyCombination >> = aShortcut [
	platform = aShortcut platform
		ifFalse: [ ^ false ].
	^ shortcut = aShortcut shortcut
]

{ #category : #iterating }
KMPlatformSpecificKeyCombination >> combinationsDo: aBlock [
	aBlock value: self
]

{ #category : #comparing }
KMPlatformSpecificKeyCombination >> hash [
	^ platform hash bitXor: shortcut hash
]

{ #category : #testing }
KMPlatformSpecificKeyCombination >> matches: aCollection [

	self platform == Smalltalk os platformFamily ifFalse: [ ^ false ].	
	^ self shortcut matches: aCollection
]

{ #category : #testing }
KMPlatformSpecificKeyCombination >> matchesCompletely: aCollection [

	self platform == Smalltalk os platformFamily ifFalse: [ ^ false ].	
	^ self shortcut matchesCompletely: aCollection
]

{ #category : #accessing }
KMPlatformSpecificKeyCombination >> platform [
	^platform
]

{ #category : #accessing }
KMPlatformSpecificKeyCombination >> platform: aPlatform [
	platform := aPlatform
]

{ #category : #printing }
KMPlatformSpecificKeyCombination >> prettyPrintOn: aStream [

	self platform = #MacOSX
		ifTrue: [ shortcut prettyPrintOn: aStream ]
		ifFalse: [ shortcut printOn: aStream ]
]

{ #category : #printing }
KMPlatformSpecificKeyCombination >> printOn: aStream [
	super printOn: aStream.
	aStream << ' ('.
	shortcut printOn: aStream.
	aStream << ' on '.
	aStream << platform asString.
	aStream << ')'
]

{ #category : #accessing }
KMPlatformSpecificKeyCombination >> shortcut [
	^shortcut
]

{ #category : #accessing }
KMPlatformSpecificKeyCombination >> shortcut: aShortcut [
	shortcut := aShortcut
]

{ #category : #combining }
KMPlatformSpecificKeyCombination >> | aShortcut [
	^KMKeyCombinationChoice withShortcuts: { self . aShortcut }
]
