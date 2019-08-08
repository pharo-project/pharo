"
SUnit tests for sockets
"
Class {
	#name : #TCPSocketTest,
	#superclass : #TestCase,
	#instVars : [
		'listenerSocket',
		'clientSocket',
		'serverSocket'
	],
	#category : #'Network-Tests-Kernel'
}

{ #category : #setup }
TCPSocketTest >> listenerAddress [
	^NetNameResolver loopBackAddress

]

{ #category : #setup }
TCPSocketTest >> listenerPort [
	^42324

]

{ #category : #running }
TCPSocketTest >> setUp [
	super setUp.
	listenerSocket := Socket newTCP listenOn: self listenerPort backlogSize: 4 interface: self listenerAddress

]

{ #category : #running }
TCPSocketTest >> tearDown [

	listenerSocket ifNotNil:[listenerSocket destroy].
	clientSocket ifNotNil:[clientSocket destroy].
	serverSocket ifNotNil:[serverSocket destroy].
	super tearDown 

]

{ #category : #tests }
TCPSocketTest >> testClientConnect [
	"Tests a client socket connection"

	clientSocket := Socket newTCP.
	clientSocket connectTo: self listenerAddress port: self listenerPort.
	clientSocket waitForConnectionFor: 2.
	self assert: clientSocket isConnected

]

{ #category : #tests }
TCPSocketTest >> testDataReceive [
	"Test data transfer and related methods"

	self testDataSending.
	"It can take a tad for the status change to be visible"
	(Delay forMilliseconds: 200) wait.
	self assert: serverSocket dataAvailable.
	self assert: serverSocket receiveData equals: 'Hello World'.
	self deny: serverSocket dataAvailable

]

{ #category : #tests }
TCPSocketTest >> testDataSending [
	"Test data transfer and related methods"

	self testServerAccept.
	clientSocket sendData: 'Hello World'.
	clientSocket waitForSendDoneFor: 2.
	self assert: clientSocket sendDone


]

{ #category : #tests }
TCPSocketTest >> testLocalAddress [
	"Tests the various localAddress values for sockets"

	self testServerAccept.
	self assert: listenerSocket localAddress asSocketAddress equals: self listenerAddress.
	self assert: clientSocket localAddress asSocketAddress equals: self listenerAddress.
	self assert: serverSocket localAddress asSocketAddress equals: self listenerAddress

]

{ #category : #tests }
TCPSocketTest >> testLocalPort [
	"Tests the various localPort values for sockets"

	self testServerAccept.
	self assert: listenerSocket localPort equals: self listenerPort.
	self assert: clientSocket localPort > 0.
	self assert: serverSocket localPort > 0

]

{ #category : #tests }
TCPSocketTest >> testRemoteAddress [
	"Tests the various remoteAddress values for sockets"

	self testServerAccept.
	self assert: listenerSocket remoteAddress equals: #[0 0 0 0].
	self assert: clientSocket remoteAddress asSocketAddress equals: self listenerAddress.
	self assert: serverSocket remoteAddress asSocketAddress equals: self listenerAddress

]

{ #category : #tests }
TCPSocketTest >> testRemotePort [
	"Tests the various remoteAddress values for sockets"

	self testServerAccept.
	self assert: listenerSocket remotePort equals: 0.
	self assert: clientSocket remotePort equals: self listenerPort.
	self assert: serverSocket remotePort > 0

]

{ #category : #tests }
TCPSocketTest >> testServerAccept [
	"Tests a server-side accept"

	self testClientConnect.
	serverSocket := listenerSocket waitForAcceptFor: 2.
	self assert: serverSocket notNil.
	self assert: serverSocket isConnected

]
