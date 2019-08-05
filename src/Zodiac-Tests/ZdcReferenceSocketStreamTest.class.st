"
ZdcReferenceSocketStreamTests test the reference SocketStream in the image
"
Class {
	#name : #ZdcReferenceSocketStreamTest,
	#superclass : #ZdcAbstractSocketStreamTest,
	#category : #'Zodiac-Tests'
}

{ #category : #testing }
ZdcReferenceSocketStreamTest class >> isAbstract [ 
	^ false
]

{ #category : #private }
ZdcReferenceSocketStreamTest >> openConnectionToHost: host port: port [
	| stream |
	stream := super openConnectionToHost: host port: port.
	self setReferenceSocketStreamOptions: stream.
	^ stream
]

{ #category : #private }
ZdcReferenceSocketStreamTest >> openConnectionToHostNamed: host port: port [
	| stream |
	stream := super openConnectionToHostNamed: host port: port.
	self setReferenceSocketStreamOptions: stream.
	^ stream
]

{ #category : #accessing }
ZdcReferenceSocketStreamTest >> socketStreamClass [
	^ SocketStream 
]

{ #category : #private }
ZdcReferenceSocketStreamTest >> socketStreamOn: socket [
	| stream |
	stream := super socketStreamOn: socket.
	self setReferenceSocketStreamOptions: stream.
	^ stream	
]
