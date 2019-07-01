"
SUnit tests for socket streams
"
Class {
	#name : #SocketStreamTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'clientStream',
		'serverStream'
	],
	#category : #'Network-Tests-Kernel'
}

{ #category : #running }
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
	serverStream := SocketStream on: serverSocket.

]

{ #category : #running }
SocketStreamTest >> tearDown [
	clientStream ifNotNil:[clientStream destroy].
	serverStream ifNotNil:[serverStream destroy].
	super tearDown 
]

{ #category : #'stream protocol' }
SocketStreamTest >> testNextIntoClose [
	"Ensure that #next:into: will function properly when the connection is closed"

	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream next: 100 into: (String new: 100) startingAt: 1) 
		equals: 'A line of text'

]

{ #category : #'stream protocol' }
SocketStreamTest >> testNextIntoCloseNonSignaling [
	"Ensure that #next:into: will function properly when the connection is closed"

	serverStream shouldSignal: false.
	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream next: 100 into: (String new: 100) startingAt: 1)
		equals: 'A line of text'

]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpTo [
	"Tests correct behavior of #upTo:"

	clientStream nextPutAll: 'A line of text', String cr, 'with more text'; flush.
	self assert: (serverStream upTo: Character cr) equals: 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPutAll: String cr; flush] fork.
	self assert: (serverStream upTo: Character cr) equals: 'with more text'
]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToAfterCloseNonSignaling [
	"Tests correct behavior of #upToAll"

	| resp |
	clientStream nextPutAll: 'A line of text'.
	clientStream close.
	serverStream shouldSignal: false.
	resp := serverStream upTo: Character cr.
	self assert: resp equals: 'A line of text'
]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToAfterCloseSignaling [
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll:'A line of text'.
	clientStream close.
	self should: [serverStream upTo: Character cr] raise: ConnectionClosed

]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToAll [
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll: 'A line of text', String crlf, 'with more text'; flush.
	self assert: (serverStream upToAll: String crlf) equals: 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPutAll: String crlf; flush] fork.
	self assert: (serverStream upToAll: String crlf) equals: 'with more text'

]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToAllAfterCloseNonSignaling [
	"Tests correct behavior of #upToAll"

	| resp |
	clientStream nextPutAll: 'A line of text'.
	clientStream close.
	serverStream shouldSignal: false.
	resp := serverStream upToAll: String crlf.
	self assert: resp equals: 'A line of text'
]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToAllAfterCloseSignaling [
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll:'A line of text'.
	clientStream close.
	self should: [serverStream upToAll: String crlf] raise: ConnectionClosed

]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToAllLimit [
	"Tests correct behavior of #upToAll:limit:"

	clientStream nextPutAll:'A line of text'; flush.
	self assert: (serverStream upToAll: String crlf limit: 5) equals: 'A line of text'
]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToAllTimeout [
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll: 'A line of text'.
	serverStream timeout: 1.
	self should: [ serverStream upToAll: String crlf ] raise: ConnectionTimedOut

]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToEndClose [
	"Ensure that #upToEnd will function properly when the connection is closed"

	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream upToEnd) 
		equals: 'A line of text'

]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToEndCloseNonSignaling [
	"Ensure that #upToEnd will function properly when the connection is closed"

	serverStream shouldSignal: false.
	clientStream nextPutAll:'A line of text'; flush.
	[(Delay forMilliseconds: 100) wait.
	clientStream close] fork.
	self assert: (serverStream upToEnd)
		equals: 'A line of text'

]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToMax [
	"Tests correct behavior of #upToAll:max:"

	clientStream nextPutAll:'A line of text'; flush.
	self assert: (serverStream upTo: Character cr limit: 5) equals: 'A line of text'
]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToSmallInteger [
	"Tests correct behavior of #upTo:"

	clientStream nextPutAll: 'A line of text', String cr, 'with more text'; flush.
	self assert: (serverStream upTo: Character cr) equals: 'A line of text'.
	[(Delay forSeconds: 1) wait.
	clientStream nextPutAll: String cr; flush] fork.
	self assert: (serverStream upTo: 13) equals: 'with more text'
]

{ #category : #'stream protocol' }
SocketStreamTest >> testUpToTimeout [
	"Tests correct behavior of #upToAll"

	clientStream nextPutAll: 'A line of text'.
	serverStream timeout: 1.
	self should: [serverStream upTo: Character cr] raise: ConnectionTimedOut
]

{ #category : #'stream protocol' }
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
