"
ZnServerConnectionClosedEvent signals that the server closed a client connection to address. 
"
Class {
	#name : 'ZnServerConnectionClosedEvent',
	#superclass : 'ZnServerLogEvent',
	#instVars : [
		'address'
	],
	#category : 'Zinc-HTTP-Logging',
	#package : 'Zinc-HTTP',
	#tag : 'Logging'
}

{ #category : 'accessing' }
ZnServerConnectionClosedEvent >> address [
	^ address
]

{ #category : 'accessing' }
ZnServerConnectionClosedEvent >> address: anObject [
	address := anObject
]

{ #category : 'printing' }
ZnServerConnectionClosedEvent >> printContentsOn: stream [
	super printContentsOn: stream.

	stream << 'Server Connection Closed '.
	self class printAddress: address on: stream
]
