"
I detect if there is a change in the layout. 
Specially if there is a change from PointerLayout to BitLayout and if there is need to propagate to the subclasses.
"
Class {
	#name : 'ShBitLayoutChanged',
	#superclass : 'ShAbstractChange',
	#category : 'Shift-Changes',
	#package : 'Shift-Changes'
}

{ #category : 'propagating' }
ShBitLayoutChanged >> hasChangeInstanceSideShape [
	^ true
]

{ #category : 'propagating' }
ShBitLayoutChanged >> propagateToSubclasses: anotherBuilder [
	anotherBuilder layoutClass: builder layoutClass
]
