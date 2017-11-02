"
ZnConnectionEstablishedEvent signals that a client HTTP connection to hostname:port (address:port) was established in duration milliseconds. This time includes DNS resolving.
"
Class {
	#name : #ZnConnectionEstablishedEvent,
	#superclass : #ZnClientLogEvent,
	#instVars : [
		'hostname',
		'port',
		'address',
		'duration',
		'proxy'
	],
	#category : 'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnConnectionEstablishedEvent >> address [
	^ address
]

{ #category : #accessing }
ZnConnectionEstablishedEvent >> address: anObject [
	address := anObject
]

{ #category : #accessing }
ZnConnectionEstablishedEvent >> duration [
	^ duration
]

{ #category : #accessing }
ZnConnectionEstablishedEvent >> duration: anObject [
	duration := anObject
]

{ #category : #accessing }
ZnConnectionEstablishedEvent >> hostname [
	^ hostname
]

{ #category : #accessing }
ZnConnectionEstablishedEvent >> hostname: anObject [
	hostname := anObject
]

{ #category : #accessing }
ZnConnectionEstablishedEvent >> port [
	^ port
]

{ #category : #accessing }
ZnConnectionEstablishedEvent >> port: anObject [
	port := anObject
]

{ #category : #printing }
ZnConnectionEstablishedEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Connection Established '; nextPutAll: hostname; nextPut: $:; print: port; space. 
	proxy ifNotNil: [ stream << 'via proxy '; print: proxy; space ].
	address do: [ :each | stream print: each ] separatedBy: [ stream nextPut: $. ].
	stream space; print: duration; << 'ms '.
]

{ #category : #accessing }
ZnConnectionEstablishedEvent >> proxy [
	^ proxy
]

{ #category : #accessing }
ZnConnectionEstablishedEvent >> proxy: anObject [
	proxy := anObject
]
