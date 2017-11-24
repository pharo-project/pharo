"
ZnEntityTooLarge is signalled when too big an entity is read from a stream.

This is a resumable exception
"
Class {
	#name : #ZnEntityTooLarge,
	#superclass : #Error,
	#category : #'Zinc-HTTP-Exceptions'
}

{ #category : #testing }
ZnEntityTooLarge >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]
