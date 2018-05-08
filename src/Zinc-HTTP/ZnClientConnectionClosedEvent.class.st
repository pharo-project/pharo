"
ZnClientConnectionClosedEvent signals that a client side connection to address:port is closed.
"
Class {
	#name : #ZnClientConnectionClosedEvent,
	#superclass : #ZnClientLogEvent,
	#instVars : [
		'address',
		'port'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnClientConnectionClosedEvent >> address [
	^ address
]

{ #category : #accessing }
ZnClientConnectionClosedEvent >> address: anObject [
	address := anObject
]

{ #category : #accessing }
ZnClientConnectionClosedEvent >> port [
	^ port
]

{ #category : #accessing }
ZnClientConnectionClosedEvent >> port: anObject [
	port := anObject
]

{ #category : #printing }
ZnClientConnectionClosedEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Connection Closed '.
	address 
		ifNil: [ 
			stream print: address ]
		ifNotNil: [ 
			address 
				do: [ :each | stream print: each ] 
				separatedBy: [ stream nextPut: $. ] ].
	stream nextPut: $:; print: port
]
