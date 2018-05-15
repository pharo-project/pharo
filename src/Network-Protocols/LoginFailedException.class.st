"
Exception for signaling login failures of protocol clients.

"
Class {
	#name : #LoginFailedException,
	#superclass : #ProtocolClientError,
	#category : #'Network-Protocols-Exceptions'
}

{ #category : #exceptiondescription }
LoginFailedException >> isResumable [
	"Resumable so we can give the user another chance to login"

	^true
]
