mocks
faultyUDPSocket

	| cls |
	
	cls := Socket newAnonymousSubclass .
	
	"the mock socket is not sending anything, so there is nothing to wait for"
	cls compile: 'waitForSendDoneFor: aNumber ^ true'.
	
	"compile the primitive to not send, but return a 0 count for any send"
	cls compile: 'primSocket: socketID sendUDPData: aStringOrByteArray toHost: hostAddress port: portNumber startIndex: startIndex count: count ^ 0'.
	
	^ cls newUDP