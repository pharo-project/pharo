"
I am showing how a slot can initialize itself on object creation.

#name => InitializedSlot default: 5.

#name => InitializedSlot defaultBlock: [ 'a bloc' ].

#name => InitializedSlot defaultBlock: [ :object | 'a bloc, parameter is the instance' ].
"
Class {
	#name : #InitializedSlot,
	#superclass : #AbstractInitializedSlot,
	#category : #'VariablesLibrary-Slots'
}

{ #category : #initialization }
InitializedSlot >> initialize: anObject [
	default isBlock 
		ifTrue: [ self write: (default cull: anObject) to: anObject ]
		ifFalse: [ self write: default to: anObject ]
]

{ #category : #'meta-object-protocol' }
InitializedSlot >> wantsInitialization [
	^ true
]
