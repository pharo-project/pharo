"
A test slot used in ShClassInstallerTest
"
Class {
	#name : 'ShTestSlot',
	#superclass : 'IndexedSlot',
	#category : 'Shift-ClassBuilder-Tests-Installer',
	#package : 'Shift-ClassBuilder-Tests',
	#tag : 'Installer'
}

{ #category : 'initialization' }
ShTestSlot >> initialize: anObject [

	super write: 'initialized' to: anObject
]

{ #category : 'initialization' }
ShTestSlot >> wantsInitialization [

	^ true
]

{ #category : 'initialization' }
ShTestSlot >> write: aValue to: anObject [
	<reflection: 'Object Modification - State modification'>
	^(self read: anObject)
		ifNil: [ super write: aValue to: anObject ]
]
