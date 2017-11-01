"
I am ZdcSimpleSocketStream.

I am a ZdcAbstractSocketStream.

I do a minimal implementation of the requirements defined by my superclass.

I rely on the rather inefficient implementation of my superclass' methods, 
funneling all access through #next and #nextPut.
"
Class {
	#name : #ZdcSimpleSocketStream,
	#superclass : #ZdcAbstractSocketStream,
	#category : 'Zodiac-Core'
}

{ #category : #'instance creation' }
ZdcSimpleSocketStream class >> openConnectionToHost: hostIP port: portNumber [
	| platformSocket |
	platformSocket := Socket newTCP.
	^ (self on: platformSocket)
		connectTo: hostIP port: portNumber;
		yourself
]

{ #category : #'instance creation' }
ZdcSimpleSocketStream class >> openConnectionToHost: hostIP port: portNumber timeout: timeout [
	| platformSocket |
	platformSocket := Socket newTCP.
	^ (self on: platformSocket)
		timeout: timeout;
		connectTo: hostIP port: portNumber;
		yourself
]

{ #category : #'instance creation' }
ZdcSimpleSocketStream class >> openConnectionToHostNamed: hostName port: portNumber [
	| platformSocket socketStream hostIP |
	platformSocket := Socket newTCP.
	socketStream := self on: platformSocket.
	hostIP := NetNameResolver addressForName: hostName timeout: socketStream timeout.
	^ socketStream 
		connectTo: hostIP port: portNumber;
		yourself
]

{ #category : #testing }
ZdcSimpleSocketStream >> atEnd [
	"I am atEnd when there is no more data to be read and there never will be.
	This means that readBuffer must be empty, there must be no more unread data 
	available at the socket, and the socket must be closed"
	
	readBuffer isEmpty ifFalse: [ ^ false ].
	(socket notNil and: [ self socketIsDataAvailable ]) 
		ifTrue: [
			"Assuming there really is data available, read it and recurse"
			self fillReadBufferNoWait.
			^ self atEnd ].   
	^ self isConnected not
]

{ #category : #initialize-release }
ZdcSimpleSocketStream >> connectTo: hostAddress port: portNumber [
	"Connect our socket to hostAddress:portNumber.
	Wait up to timeout for a connection"
	
	self socketConnectTo: hostAddress port: portNumber
]

{ #category : #'private in' }
ZdcSimpleSocketStream >> fillBytes: bytes startingAt: offset count: count [
	"Ask the socket to read count elements into bytes starting at offset. Do not wait. Return read count."
	
	self isConnected ifFalse: [ ConnectionClosed signal: 'Cannot read data' ].  
	^ self 
		socketReceiveDataInto: bytes 
		startingAt: offset 
		count: count
]

{ #category : #'private in' }
ZdcSimpleSocketStream >> fillReadBuffer [
	"Ask the socket to fill the read buffer with data. Wait for a data."
	
	self fillReadBufferNoWait.
	(readBuffer isEmpty and: [ self isConnected ])
		ifTrue: [ 
			[ self socketWaitForData ] on: ConnectionClosed do: [ ^ self ]. "when successful, recurse, else signal exception"
			self fillReadBuffer ]  
]

{ #category : #'private in' }
ZdcSimpleSocketStream >> fillReadBufferNoWait [
	"Ask the socket to fill the read buffer with data. Do not wait for data."
	
	| readCount |
	readBuffer compact.
	readCount := self 
		fillBytes: readBuffer buffer 
		startingAt: readBuffer freeSpaceStart
		count: readBuffer availableForWriting .
	readBuffer advanceWritePointer: readCount.
	^ readCount
]

{ #category : #'private out' }
ZdcSimpleSocketStream >> flushBytes: bytes startingAt: offset count: count [
	"Ask the socket to write count bytes starting from offset. Wait. Fail if not successful"
	
	| writeCount |
	self isConnected ifFalse: [ ConnectionClosed signal: 'Cannot write data' ].  
	writeCount := 0.
	[ | written |
		written := self socketSendData: bytes startingAt: offset + writeCount count: count - writeCount.
		(writeCount := writeCount + written) = count ] whileFalse: [
		self socketWaitForSendDone
			ifFalse: [ ConnectionTimedOut signal: 'Data send timed out.' ] ]
]

{ #category : #'private out' }
ZdcSimpleSocketStream >> flushWriteBuffer [
	"Ask the socket to write all data in the write buffer. Fail if not successful"
	
	writeBuffer isEmpty ifTrue: [ ^ self ].  
	self 
		flushBytes: writeBuffer buffer
		startingAt: writeBuffer contentsStart
		count: writeBuffer availableForReading. 
	writeBuffer reset 
]

{ #category : #testing }
ZdcSimpleSocketStream >> isConnected [
	"Are we connected at the socket level ?"
	
	^ socket notNil and: [ self socketIsConnected ]
]

{ #category : #testing }
ZdcSimpleSocketStream >> isDataAvailable [
	"Return true when there is data available for reading.
	This does not block."
	
	readBuffer isEmpty ifFalse: [ ^ true ].
	(socket notNil and: [ self socketIsDataAvailable ]) 
		ifTrue: [
			"Assuming there really is data available, read it and recurse"
			self fillReadBufferNoWait.
			^ self isDataAvailable ].   
	^ false
]
