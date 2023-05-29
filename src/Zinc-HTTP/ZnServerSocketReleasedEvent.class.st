"
ZnServerSocketReleasedEvent signals a new server socket on address:port is released (closed).
"
Class {
	#name : #ZnServerSocketReleasedEvent,
	#superclass : #ZnServerLogEvent,
	#instVars : [
		'address',
		'port'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnServerSocketReleasedEvent >> address [
	^ address
]

{ #category : #accessing }
ZnServerSocketReleasedEvent >> address: anObject [
	address := anObject
]

{ #category : #accessing }
ZnServerSocketReleasedEvent >> port [
	^ port
]

{ #category : #accessing }
ZnServerSocketReleasedEvent >> port: anObject [
	port := anObject
]

{ #category : #printing }
ZnServerSocketReleasedEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Server Socket Released '.
	address do: [ :each | stream print: each ] separatedBy: [ stream nextPut: $. ].
	stream nextPut: $:; print: port
]
