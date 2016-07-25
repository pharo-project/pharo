tests
testUDPFaultySend

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
		
	] notTakeMoreThanMilliseconds: 20 .

	socket destroy.