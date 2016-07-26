tests
testUDPBroadcastError
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