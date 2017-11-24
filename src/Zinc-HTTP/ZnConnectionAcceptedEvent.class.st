"
ZnConnectionAcceptedEvent is signalled when an HTTP server accepts an incoming connection from a client at address.
"
Class {
	#name : #ZnConnectionAcceptedEvent,
	#superclass : #ZnServerLogEvent,
	#instVars : [
		'address'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnConnectionAcceptedEvent >> address [
	^ address
]

{ #category : #accessing }
ZnConnectionAcceptedEvent >> address: anObject [
	address := anObject
]

{ #category : #printing }
ZnConnectionAcceptedEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Connection Accepted '.
	address do: [ :each | stream print: each ] separatedBy: [ stream nextPut: $. ]
]
