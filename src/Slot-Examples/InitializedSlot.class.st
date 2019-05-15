"
I am showing how a slot can initialize itself on object creation.

#name => InitializedSlot default: 5.

#name => InitializedSlot defaultBlock: [ 'a bloc' ].

#name => InitializedSlot defaultBlock: [ :object | 'a bloc, parameter is the instance' ].
"
Class {
	#name : #InitializedSlot,
	#superclass : #AbstractInitializedSlot,
	#category : #'Slot-Examples-Base'
}

{ #category : #initialization }
InitializedSlot >> initialize: anObject [
	self write: (default copy cull: anObject) to: anObject
]

{ #category : #'meta-object-protocol' }
InitializedSlot >> wantsInitalization [
	^ true
]
