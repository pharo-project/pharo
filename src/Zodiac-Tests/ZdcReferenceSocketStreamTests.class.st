"
ZdcReferenceSocketStreamTests test the reference SocketStream in the image
"
Class {
	#name : #ZdcReferenceSocketStreamTests,
	#superclass : #ZdcAbstractSocketStreamTests,
	#category : #Zodiac-Tests
}

{ #category : #testing }
ZdcReferenceSocketStreamTests class >> isAbstract [ 
	^ false
]

{ #category : #private }
ZdcReferenceSocketStreamTests >> openConnectionToHost: host port: port [
	| stream |
	stream := super openConnectionToHost: host port: port.
	self setReferenceSocketStreamOptions: stream.
	^ stream
]

{ #category : #private }
ZdcReferenceSocketStreamTests >> openConnectionToHostNamed: host port: port [
	| stream |
	stream := super openConnectionToHostNamed: host port: port.
	self setReferenceSocketStreamOptions: stream.
	^ stream
]

{ #category : #accessing }
ZdcReferenceSocketStreamTests >> socketStreamClass [
	^ SocketStream 
]

{ #category : #private }
ZdcReferenceSocketStreamTests >> socketStreamOn: socket [
	| stream |
	stream := super socketStreamOn: socket.
	self setReferenceSocketStreamOptions: stream.
	^ stream	
]
