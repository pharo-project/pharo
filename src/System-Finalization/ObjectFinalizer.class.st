"
Represents an action that is to be taken when a target object (not referenced by the ObjectFinalizer) is garbage-collected.
"
Class {
	#name : #ObjectFinalizer,
	#superclass : #MessageSend,
	#category : #System-Finalization
}

{ #category : #finalization }
ObjectFinalizer >> finalize [
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO garantuee that the resource associated with the receiver hasn't been free'd before so take care that you don't run into trouble - this all may happen with interrupt priority."
	self value
]

{ #category : #initialize }
ObjectFinalizer >> receiver: anObject selector: aSymbol argument: aParameter [
	"Not really necessary now that we're a subclass of MessageSend, but we keep it around because existing code might expect it."
	self 
		receiver: anObject;
		selector: aSymbol;
		arguments: (Array with: aParameter).
]
