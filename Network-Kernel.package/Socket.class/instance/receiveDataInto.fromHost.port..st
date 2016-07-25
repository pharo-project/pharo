datagrams
receiveDataInto: aStringOrByteArray fromHost: hostAddress port: portNumber
	"Receive a UDP packet from the given hostAddress/portNumber, storing the data in the given buffer, and return the number of bytes received. Note the given buffer may be only partially filled by the received data."

	[ | datagram |
		datagram := self receiveUDPDataInto: aStringOrByteArray.
		((datagram at: 2) = hostAddress and: [ (datagram at: 3) = portNumber ]) 
			ifTrue: [ ^ datagram at: 1 ]
			ifFalse: [ ^0 ] ] repeat