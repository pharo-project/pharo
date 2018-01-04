"
I detect if there is a change in the layout. 
Specially if there is a change from PointerLayout to BitLayout and if there is need to propagate to the subclasses.
"
Class {
	#name : #ShBitLayoutChanged,
	#superclass : #ShAbstractChange,
	#category : 'Shift-Changes'
}

{ #category : #propagating }
ShBitLayoutChanged >> propagateToSubclasses: anotherBuilder [
	anotherBuilder layoutDefinition layoutClass: builder layoutDefinition layoutClass 
]
