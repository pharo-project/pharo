"
A ProtocolRemovalException is an error raised when someone try to remove a protocol that should not be removed
"
Class {
	#name : #ProtocolRemovalException,
	#superclass : #Error,
	#category : #Deprecated10
}

{ #category : #testing }
ProtocolRemovalException class >> isDeprecated [
	"exception for this made no sense, usage was removed"
	^true
]
