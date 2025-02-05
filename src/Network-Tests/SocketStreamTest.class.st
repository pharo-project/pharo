"
SUnit tests for socket streams
"
Class {
	#name : 'SocketStreamTest',
	#superclass : 'ClassTestCase',
	#instVars : [
		'clientStream',
		'serverStream'
	],
	#category : 'Network-Tests-Kernel',
	#package : 'Network-Tests',
	#tag : 'Kernel'
}

{ #category : 'coverage' }
SocketStreamTest >> classToBeTested [

	^ SocketStream
]

{ #category : 'stream protocol' }
SocketStreamTest >> closeServerAndSendOnceFromClient [

	serverStream close.
	"The first send after the other end is closed will not have problems as long as it fits within the send buffer."
	self
		shouldnt: [
			clientStream
				nextPutAll: 'A line of text';
				flush ]
		raise: NetworkError.
	"Wait for the state of the underlying socket to update--normally this happens near-instantly,
	but when running tests in a batch of other sockets-related tests, e.g. in CI, often a subsequent send
	will still succeed without the wait, causing the test to fail."
	(Delay forMilliseconds: 100) wait
]

{ #category : 'running' }
SocketStreamTest >> setUp [
	| listener clientSocket serverSocket |
	super setUp.
	listener := Socket newTCP.
	[listener listenOn: 0 backlogSize: 4.

	clientSocket := Socket newTCP.
	clientSocket connectTo: #[127 0 0 1] port: listener localPort.
	clientSocket waitForConnectionFor: 1.
	self assert: clientSocket isConnected.

	serverSocket := listener waitForAcceptFor: 1.
	self assert: serverSocket isConnected.
	] ensure:[listener destroy].

	clientStream := SocketStream on: clientSocket.
	serverStream := SocketStream on: serverSocket
]

{ #category : 'running' }
SocketStreamTest >> tearDown [
	clientStream ifNotNil:[clientStream destroy].
	serverStream ifNotNil:[serverStream destroy].
	super tearDown
]

{ #category : 'stream protocol' }
SocketStreamTest >> testFlushLargeMessageOtherEndClosed [
	"A large send should be broken into chunks and fail once the TCP send buffer
	is full. Ensure we actually exceed the size of the send buffer, but also try
	to reduce it first so we don't need an excessively large message. Some platforms
	(Linux) have minimum values for this setting that prevent us from relying on
	lowering it alone. Attempt to set a 65KiB buffer and double whatever we get."

	| bufferSize |
	"Windows seems to accept arbitrarily large send() payloads regardless of the
	send buffer size, so we can't force a wait other than by calling #flush twice,
	which would defeat the whole purpose of the test.
	Skip on Windows for now as this is not a regression."
	OSPlatform current isWindows ifTrue: [ self skip ].
	clientStream socket setOption: 'SO_SNDBUF' value: 2 ** 16.
	bufferSize := (clientStream socket getOption: 'SO_SNDBUF') second.
	serverStream close.
	self
		should: [
			clientStream
				nextPutAll: (String new: bufferSize * 2 withAll: $a);
				flush ]
		raise: ConnectionClosed
]

{ #category : 'stream protocol' }
SocketStreamTest >> testFlushOtherEndClosed [
	"Ensure that #flush properly raises an error when called when the other end is closed."

	self closeServerAndSendOnceFromClient.
	self
		should: [
			clientStream
				nextPutAll: 'Another line of text';
				flush ]
		raise: ConnectionClosed
]

{ #category : 'stream protocol' }
SocketStreamTest >> testNextIntoClose [
	"Ensure that #next:into: will function properly when the connection is closed"

	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream next: 100 into: (String new: 100) startingAt: 1)
		equals: 'A line of text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testNextIntoCloseNonSignaling [
	"Ensure that #next:into: will function properly when the connection is closed"

	serverStream shouldSignal: false.
	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream next: 100 into: (String new: 100) startingAt: 1)
		equals: 'A line of text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testNextPutAllFlushOtherEndClosed [
	"#nextPutAllFlush: does its own error handling, so needs to be tested separately."

	self closeServerAndSendOnceFromClient.
	self
		should: [ clientStream nextPutAllFlush: 'Another line of text' ]
		raise: ConnectionClosed
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpTo [
	"Tests correct behavior of #upTo:"

	clientStream nextPutAll: 'A line of text', String cr, 'with more text'; flush.
	self assert: (serverStream upTo: Character cr) equals: 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPutAll: String cr; flush] fork.
	self assert: (serverStream upTo: Character cr) equals: 'with more text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToAfterCloseNonSignaling [
	"Tests correct behavior of #upToAll"

	| resp |
	"Since we changed the machine for the CI this test is breaking on windows. Skipping it temporarily."
	OSPlatform current isWindows ifTrue: [ self skip ].
	clientStream nextPutAll: 'A line of text'.
	clientStream close.
	serverStream shouldSignal: false.
	resp := serverStream upTo: Character cr.
	self assert: resp equals: 'A line of text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToAfterCloseSignaling [
	"Tests correct behavior of #upToAll"

	"Since we changed the machine for the CI this test is breaking on windows. Skipping it temporarily."

	OSPlatform current isWindows ifTrue: [ self skip ].
	clientStream nextPutAll: 'A line of text'.
	clientStream close.
	self should: [ serverStream upTo: Character cr ] raise: ConnectionClosed
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToAll [
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll: 'A line of text', String crlf, 'with more text'; flush.
	self assert: (serverStream upToAll: String crlf) equals: 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPutAll: String crlf; flush] fork.
	self assert: (serverStream upToAll: String crlf) equals: 'with more text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToAllAfterCloseNonSignaling [
	"Tests correct behavior of #upToAll"

	| resp |
	"Since we changed the machine for the CI this test is breaking on windows. Skipping it temporarily."
	OSPlatform current isWindows ifTrue: [ self skip ].
	clientStream nextPutAll: 'A line of text'.
	clientStream close.
	serverStream shouldSignal: false.
	resp := serverStream upToAll: String crlf.
	self assert: resp equals: 'A line of text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToAllAfterCloseSignaling [
	"Tests correct behavior of #upToAll"

	"Since we changed the machine for the CI this test is breaking on windows. Skipping it temporarily."

	OSPlatform current isWindows ifTrue: [ self skip ].
	clientStream nextPutAll: 'A line of text'.
	clientStream close.
	self should: [ serverStream upToAll: String crlf ] raise: ConnectionClosed
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToAllLimit [
	"Tests correct behavior of #upToAll:limit:"

	clientStream nextPutAll:'A line of text'; flush.
	self assert: (serverStream upToAll: String crlf limit: 5) equals: 'A line of text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToAllTimeout [
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll: 'A line of text'.
	serverStream timeout: 1.
	self should: [ serverStream upToAll: String crlf ] raise: ConnectionTimedOut
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToEndClose [
	"Ensure that #upToEnd will function properly when the connection is closed"

	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream upToEnd)
		equals: 'A line of text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToEndCloseNonSignaling [
	"Ensure that #upToEnd will function properly when the connection is closed"

	serverStream shouldSignal: false.
	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream upToEnd)
		equals: 'A line of text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToMax [
	"Tests correct behavior of #upToAll:max:"

	clientStream nextPutAll:'A line of text'; flush.
	self assert: (serverStream upTo: Character cr limit: 5) equals: 'A line of text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToSmallInteger [
	"Tests correct behavior of #upTo:"

	clientStream nextPutAll: 'A line of text', String cr, 'with more text'; flush.
	self assert: (serverStream upTo: Character cr) equals: 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPutAll: String cr; flush] fork.
	self assert: (serverStream upTo: 13) equals: 'with more text'
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToTimeout [
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll: 'A line of text'.
	serverStream timeout: 1.
	self should: [serverStream upTo: Character cr] raise: ConnectionTimedOut
]

{ #category : 'stream protocol' }
SocketStreamTest >> testUpToWithByteArray [
	"Tests correct behavior of #upTo:"

	|byteArray|

	byteArray := ('A line of text', String cr, 'with more text') asByteArray.
	clientStream binary.
	serverStream binary.
	clientStream nextPutAll: byteArray; flush.
	self assert: (serverStream upTo: Character cr) asString equals: 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPut: Character cr; flush] fork.
	self assert: (serverStream upTo: Character cr) asString equals: 'with more text'
]
