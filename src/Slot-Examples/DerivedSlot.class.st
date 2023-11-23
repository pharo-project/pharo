"
A derived slot is a virtual slot that stores its value in a hidden base slot.
"
Class {
	#name : 'DerivedSlot',
	#superclass : 'Slot',
	#instVars : [
		'baseSlot'
	],
	#category : 'Slot-Examples-Base',
	#package : 'Slot-Examples',
	#tag : 'Base'
}

{ #category : 'testing' }
DerivedSlot >> isAccessedIn: aMethod [
	"Return whether the receiver is accessed in the arg. Pay attention that the compiler is free to remove unused variable so the access is only garanteed for used instance variables e.g. part of return, expression, assignment..."
	
	^ (baseSlot isAccessedIn: aMethod)
		  ifTrue: [ super isAccessedIn: aMethod ]
		  ifFalse: [ false ]
]

{ #category : 'testing' }
DerivedSlot >> isReadIn: aMethod [
	(baseSlot isReadIn: aMethod) ifFalse: [ ^false ].
	^super isReadIn: aMethod
]

{ #category : 'testing' }
DerivedSlot >> isWrittenIn: aMethod [
	(baseSlot isWrittenIn: aMethod) ifFalse: [ ^false ].
	^super isWrittenIn: aMethod
]
