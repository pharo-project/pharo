Class {
	#name : #UDPSocketTest,
	#superclass : #TestCase,
	#instVars : [
		'listenerSocket',
		'clientSocket',
		'serverSocket'
	],
	#category : #'Network-Tests-Kernel'
}

{ #category : #support }
UDPSocketTest >> faultyUDPSocket [

	| cls |
	
	cls := Socket newAnonymousSubclass .
	
	"the mock socket is not sending anything, so there is nothing to wait for"
	cls compile: 'waitForSendDoneFor: aNumber ^ true'.
	
	"compile the primitive to not send, but return a 0 count for any send"
	cls compile: 'primSocket: socketID sendUDPData: aStringOrByteArray toHost: hostAddress port: portNumber startIndex: startIndex count: count ^ 0'.
	
	^ cls newUDP
]

{ #category : #tests }
UDPSocketTest >> testUDPBroadcastError [
	"Test that we get a specific error when failure is due to sending to a broadcast address without SO_BROADCAST set"
	"Use 255.255.255.255 for testing, which in RFC 919 is defined as 'denoting a broadcast on a local hardware network, which must not be forwarded. 
	This address may be used, for example, by hosts that do not know their network number and are asking some server for it.'"

	self
		should: [ 
			Socket newUDP
				setOption: 'SO_BROADCAST' value: false;
				sendUDPData: #[] toHost: #[255 255 255 255] port: 1950 ]
		raise: NoBroadcastAllowed.
	self
		shouldnt: [ 
			Socket newUDP
				setOption: 'SO_BROADCAST' value: true;
				sendUDPData: #[] toHost: #[255 255 255 255] port: 1 ]
		raise: NoBroadcastAllowed
]

{ #category : #tests }
UDPSocketTest >> testUDPFaultySend [

	| socket host |
	
	socket := self faultyUDPSocket.	
	host := #[1 2 3 4].
	
	"Check that sending is not caught in a infinite loop"
	self should: [ 
		
		self 
			deny: (socket isBroadcastAddress: host);
			should: [socket sendUDPData: #[123] toHost: host port: 1 ] raise: NetworkError.
			
		host := #[ 255 255 255 255].
		
		self 
			assert: (socket isBroadcastAddress: host);
			should: [ socket sendUDPData: #[123] toHost: host port: 1 ] raise: NoBroadcastAllowed.
			
		socket setOption: 'SO_BROADCAST' value: true.
		
		self should: [ socket sendUDPData: #[123] toHost: host port: 1 ] raise: NetworkError.
		
	] notTakeMoreThanMilliseconds: 20.

	socket destroy
]
