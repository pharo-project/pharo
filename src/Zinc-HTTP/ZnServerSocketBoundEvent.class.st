"
ZnServerSocketBoundEvent signals a new server socket on address:port is bound (opened).
"
Class {
	#name : #ZnServerSocketBoundEvent,
	#superclass : #ZnServerLogEvent,
	#instVars : [
		'address',
		'port'
	],
	#category : 'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnServerSocketBoundEvent >> address [
	^ address
]

{ #category : #accessing }
ZnServerSocketBoundEvent >> address: anObject [
	address := anObject
]

{ #category : #accessing }
ZnServerSocketBoundEvent >> port [
	^ port
]

{ #category : #accessing }
ZnServerSocketBoundEvent >> port: anObject [
	port := anObject
]

{ #category : #printing }
ZnServerSocketBoundEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Server Socket Bound '.
	address do: [ :each | stream print: each ] separatedBy: [ stream nextPut: $. ].
	stream nextPut: $:; print: port
	
]
