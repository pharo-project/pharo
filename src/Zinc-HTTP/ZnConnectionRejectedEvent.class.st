"
ZnConnectionRejectedEvent is signalled when an HTTP server rejects an incoming connection from a client at address.
"
Class {
	#name : 'ZnConnectionRejectedEvent',
	#superclass : 'ZnServerLogEvent',
	#instVars : [
		'address'
	],
	#category : 'Zinc-HTTP-Logging',
	#package : 'Zinc-HTTP',
	#tag : 'Logging'
}

{ #category : 'accessing' }
ZnConnectionRejectedEvent >> address [
	^ address
]

{ #category : 'accessing' }
ZnConnectionRejectedEvent >> address: anObject [
	address := anObject
]

{ #category : 'printing' }
ZnConnectionRejectedEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << 'Connection Rejected'.
	address ifNotNil: [
		stream space.
		address do: [ :each | stream print: each ] separatedBy: [ stream nextPut: $. ] ]
]
