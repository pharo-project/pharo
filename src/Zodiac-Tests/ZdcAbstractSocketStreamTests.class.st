"
ZdcAbstractSocketStreamTests holds units tests for socket streams.

This is an abstract class, subclasses should implement #socketStreamClass 
"
Class {
	#name : #ZdcAbstractSocketStreamTests,
	#superclass : #TestCase,
	#category : #'Zodiac-Tests'
}

{ #category : #testing }
ZdcAbstractSocketStreamTests class >> isAbstract [ 
	^ true
]

{ #category : #accessing }
ZdcAbstractSocketStreamTests >> bytes: size [
	| count data pattern |
	count := 0.
	pattern := (0 to: 255) asByteArray.
	data := ByteArray streamContents: [ :stream |
		[ count < size ] whileTrue: [
			stream nextPutAll: pattern.
			count := count + pattern size ] ].
	^ data copyFrom: 1 to: size     
]

{ #category : #accessing }
ZdcAbstractSocketStreamTests >> listenBacklogSize [
	"Server socket backlog size (number of pending connection waiting to be accepted)"
	
	^ 32
]

{ #category : #private }
ZdcAbstractSocketStreamTests >> openConnectionToHost: host port: port [
	^ self socketStreamClass 
		openConnectionToHost: host port: port
]

{ #category : #private }
ZdcAbstractSocketStreamTests >> openConnectionToHostNamed: host port: port [
	^ self socketStreamClass 
		openConnectionToHostNamed: host port: port
]

{ #category : #accessing }
ZdcAbstractSocketStreamTests >> port [ 
	^ 1701
]

{ #category : #private }
ZdcAbstractSocketStreamTests >> referenceSocketStreamOn: socket [
	| stream |
	stream := SocketStream on: socket.
	self setReferenceSocketStreamOptions: stream.
	^ stream
]

{ #category : #private }
ZdcAbstractSocketStreamTests >> runClient: block [
	| semaphore |
	semaphore := Semaphore new.
	[ block ensure: [ semaphore signal ] ] forkAt: self serverPriority - 1.
	semaphore wait
]

{ #category : #private }
ZdcAbstractSocketStreamTests >> runServer: block [
	| serverSocket semaphore |
	serverSocket := self serverSocketOn: self port.
	self assert: serverSocket notNil.
	self assert: serverSocket localPort equals: self port.
	semaphore := Semaphore new.
	[ 
	semaphore signal.
	[ block cull: serverSocket cull: semaphore ]
		ensure: [ 
			serverSocket closeAndDestroy.
			semaphore signal ] ] forkAt: self serverPriority.
	semaphore wait.
	^ semaphore
]

{ #category : #private }
ZdcAbstractSocketStreamTests >> serverPriority [
	^ Processor userBackgroundPriority
]

{ #category : #private }
ZdcAbstractSocketStreamTests >> serverSocketOn: port [
	| socket |
	(socket := Socket newTCP)
		setOption: 'TCP_NODELAY' value: 1;
		setOption: 'SO_SNDBUF' value: self socketBufferSize;
		setOption: 'SO_RCVBUF' value: self socketBufferSize .
	socket listenOn: port backlogSize: self listenBacklogSize.
	socket isValid
		ifFalse: [ self error: 'Cannot create socket on port ', port printString ].
	^ socket
]

{ #category : #private }
ZdcAbstractSocketStreamTests >> setReferenceSocketStreamOptions: stream [
	stream
		binary;
		shouldSignal: true;
		autoFlush: false;
		bufferSize: self socketBufferSize;
		timeout: self socketStreamTimeout
]

{ #category : #accessing }
ZdcAbstractSocketStreamTests >> socketBufferSize [
	"Size in bytes for Sockets and SocketStream IO buffers"

	^ 4096
]

{ #category : #accessing }
ZdcAbstractSocketStreamTests >> socketStreamClass [
	^ self subclassResponsibility 
]

{ #category : #private }
ZdcAbstractSocketStreamTests >> socketStreamOn: socket [
	^ self socketStreamClass 
		on: socket
]

{ #category : #accessing }
ZdcAbstractSocketStreamTests >> socketStreamTimeout [
	"Timeout in seconds for SocketStream IO"
	
	^ ZnNetworkingUtils socketStreamTimeout
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testAddOneEcho [
	| dataSent dataRead clientStream semaphore |
	semaphore := self runServer: [ :serverSocket | | clientSocket stream data |
		clientSocket := serverSocket waitForAcceptFor: 10.
		stream := self socketStreamOn: clientSocket.
		data := stream next.
		stream nextPut: ((data + 1) bitAnd: 16rff).  
		stream close ].
	dataSent := 99.
	clientStream := self openConnectionToHostNamed: 'localhost' port: self port.
	clientStream nextPut: dataSent; flush.
	dataRead := clientStream next.
	clientStream close.
	self assert: dataRead equals: dataSent + 1.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testPlainClientRead [
	| dataSent dataRead clientStream semaphore |
	dataSent := #[ 6 5 4 3 2 1 ].
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		serverStream nextPutAll: dataSent.
		serverStream close.
		mySemaphore signal ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	dataRead := clientStream upToEnd.
	clientStream close.
	semaphore wait.
	self assert: dataSent equals: dataRead.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testPlainClientRead10k [
	| dataSent dataRead clientStream semaphore |
	dataSent := self bytes: 10000.
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		serverStream nextPutAll: dataSent.
		serverStream close.
		mySemaphore signal ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	dataRead := clientStream upToEnd.
	clientStream close.
	semaphore wait.
	self assert: dataSent equals: dataRead.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testPlainClientRead10kInPieces1 [
	| dataSent dataRead clientStream semaphore |
	dataSent := self bytes: 10000.
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		serverStream nextPutAll: dataSent.
		serverStream close.
		mySemaphore signal ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	dataRead := ByteArray new: 10000 streamContents: [ :stream |
		10 timesRepeat: [ stream nextPutAll: (clientStream next: 1000) ] ].
	clientStream close.
	semaphore wait.
	self assert: dataSent equals: dataRead.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testPlainClientRead10kInPieces2 [
	| dataSent dataRead clientStream semaphore |
	dataSent := self bytes: 10000.
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		serverStream nextPutAll: dataSent.
		serverStream close.
		mySemaphore signal ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	dataRead := ByteArray new: 10000.
	0 to: 9 do: [ :index | 
		clientStream next: 1000 into: dataRead startingAt: (index * 1000) + 1 ].
	clientStream close.
	semaphore wait.
	self assert: dataSent equals: dataRead.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testPlainClientSkip [
	| dataSent dataRead clientStream semaphore |
	dataSent := #[ 6 5 4 3 2 1 ].
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		serverStream nextPut: 0; nextPutAll: dataSent.
		serverStream close.
		mySemaphore signal ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	dataRead := clientStream skip: 1; upToEnd.
	clientStream close.
	semaphore wait.
	self assert: dataSent equals: dataRead.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testPlainClientSkip10k [
	| dataSent dataRead clientStream semaphore |
	dataSent := #[ 6 5 4 3 2 1 ].
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		10000 timesRepeat: [ serverStream nextPut: 0 ].  
		serverStream nextPutAll: dataSent.
		serverStream close.
		mySemaphore signal ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	dataRead := clientStream skip: 10000; upToEnd.
	clientStream close.
	semaphore wait.
	self assert: dataSent equals: dataRead.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testPlainClientWrite [
	| dataSent dataRead clientStream semaphore |
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		dataRead := serverStream upToEnd.
		serverStream close.
		mySemaphore signal ].
	dataSent := #[ 1 2 3 4 5 6 ].
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	clientStream nextPutAll: dataSent.
	clientStream close.
	semaphore wait.
	self assert: dataSent equals: dataRead.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testPlainClientWrite10k [
	| dataSent dataRead clientStream semaphore |
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		dataRead := serverStream upToEnd.
		serverStream close.
		mySemaphore signal ].
	dataSent := self bytes: 10000.
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	clientStream nextPutAll: dataSent.
	clientStream close.
	semaphore wait.
	self assert: dataSent equals: dataRead.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testPlainClientWrite10kInPieces [
	| dataSent dataRead clientStream semaphore |
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket serverStream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		serverStream := self referenceSocketStreamOn: clientSocket.
		dataRead := serverStream upToEnd.
		serverStream close.
		mySemaphore signal ].
	dataSent := self bytes: 10000.
	clientStream := self openConnectionToHost: #[ 127 0 0 1 ] port: self port.
	0 to: 9 do: [ :index |
		clientStream next: 1000 putAll: dataSent startingAt: (1000 * index) + 1 ].
	clientStream close.
	semaphore wait.
	self assert: dataSent equals: dataRead.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testReverseEcho [
	| dataSent dataRead clientStream semaphore data |
	semaphore := self runServer: [ :serverSocket | | clientSocket stream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		stream := self socketStreamOn: clientSocket.
		data := stream upTo: 0.
		stream nextPutAll: data reverse; nextPut: 0.  
		stream close ].
	dataSent := #[ 1 2 3 4 5 6 ].
	clientStream := self openConnectionToHostNamed: 'localhost' port: self port.
	clientStream nextPutAll: dataSent; nextPut: 0; flush.
	dataRead := clientStream upTo: 0.
	clientStream close.
	self assert: dataRead equals: dataSent reverse.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testReverseEcho10kFixed [
	| dataSent dataRead clientStream semaphore |
	semaphore := self runServer: [ :serverSocket | | clientSocket stream data |
		clientSocket := serverSocket waitForAcceptFor: 10.
		stream := self socketStreamOn: clientSocket.
		data := stream next: 10000.
		stream nextPutAll: data reverse.  
		stream close ].
	dataSent := self bytes: 10000.
	clientStream := self openConnectionToHostNamed: 'localhost' port: self port.
	clientStream nextPutAll: dataSent; flush.
	dataRead := clientStream next: 10000.
	clientStream close.
	self assert: dataRead equals: dataSent reverse.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testReverseEcho10kSearch [
	| dataSent dataRead clientStream semaphore |
	semaphore := self runServer: [ :serverSocket | | clientSocket stream data |
		clientSocket := serverSocket waitForAcceptFor: 10.
		stream := self socketStreamOn: clientSocket.
		data := stream upTo: 0.
		stream nextPutAll: data reverse; nextPut: 0.  
		stream close ].
	dataSent := (self bytes: 10000) replace: [ :each | each max: 1 ].
	clientStream := self openConnectionToHostNamed: 'localhost' port: self port.
	clientStream nextPutAll: dataSent; nextPut: 0; flush.
	dataRead := clientStream upTo: 0.
	clientStream close.
	self assert: dataRead equals: dataSent reverse.
	semaphore wait
]

{ #category : #testing }
ZdcAbstractSocketStreamTests >> testReverseEchoUpToEnd [
	| dataSent dataRead clientStream semaphore data |
	semaphore := self runServer: [ :serverSocket :mySemaphore | | clientSocket stream |
		clientSocket := serverSocket waitForAcceptFor: 10.
		stream := self socketStreamOn: clientSocket.
		"Doing #upToEnd here does not seem to work"
		data := stream next: 6.
		stream nextPutAll: data reverse.  
		stream close ].
	self runClient: [ 
		dataSent := #[ 1 2 3 4 5 6 ].
		clientStream := self openConnectionToHostNamed: 'localhost' port: self port.
		clientStream nextPutAll: dataSent; flush.
		dataRead := clientStream upToEnd.
		clientStream close.
		self assert: dataRead equals: dataSent reverse.
		semaphore wait ]
]
