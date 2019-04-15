"
A derived slot is a virtual slot that stores its value in a hidden base slot.
"
Class {
	#name : #DerivedSlot,
	#superclass : #Slot,
	#instVars : [
		'baseSlot'
	],
	#category : #'Slot-Examples-Base'
}

{ #category : #testing }
DerivedSlot >> isAccessedIn: aMethod [
	(baseSlot isAccessedIn: aMethod) ifFalse: [ ^false ].
	^super isAccessedIn: aMethod
]

{ #category : #testing }
DerivedSlot >> isReadIn: aMethod [
	(baseSlot isReadIn: aMethod) ifFalse: [ ^false ].
	^super isReadIn: aMethod
]

{ #category : #testing }
DerivedSlot >> isWrittenIn: aMethod [
	(baseSlot isWrittenIn: aMethod) ifFalse: [ ^false ].
	^super isWrittenIn: aMethod
]
