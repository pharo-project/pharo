"
A test slot used in ShClassInstallerTest
"
Class {
	#name : #ShTestSlot,
	#superclass : #IndexedSlot,
	#category : #'Shift-ClassInstaller-Tests'
}

{ #category : #initialization }
ShTestSlot >> initialize: anObject [

	super write: 'initialized' to: anObject
]

{ #category : #initialization }
ShTestSlot >> wantsInitialization [

	^ true
]

{ #category : #initialization }
ShTestSlot >> write: aValue to: anObject [

	^(self read: anObject)
		ifNil: [ super write: aValue to: anObject ]
]
