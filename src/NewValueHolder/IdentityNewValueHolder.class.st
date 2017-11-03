"
I am a special because I announce changes only if the new value set is different from the current value.
"
Class {
	#name : #IdentityNewValueHolder,
	#superclass : #NewValueHolder,
	#category : #NewValueHolder
}

{ #category : #accessing }
IdentityNewValueHolder >> value: anObject [

	self value == anObject ifTrue: [ ^ anObject ].
	super value: anObject
]
