"
ZnTooManyRedirects is signalled when an HTTP client has been following more redirects than allowed.

Part of Zinc HTTP Components. 
"
Class {
	#name : #ZnTooManyRedirects,
	#superclass : #Error,
	#category : 'Zinc-HTTP-Exceptions'
}

{ #category : #testing }
ZnTooManyRedirects >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]
