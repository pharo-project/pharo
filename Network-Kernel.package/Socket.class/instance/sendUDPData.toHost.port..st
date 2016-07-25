datagrams
sendUDPData: aStringOrByteArray toHost: hostAddress port: portNumber
	"Send a UDP packet containing the given data to the specified host/port."
	| bytesToSend bytesSent count |

	bytesToSend := aStringOrByteArray size.
	bytesSent := 0.
	[
		(self waitForSendDoneFor: 20)
			ifFalse: [ConnectionTimedOut signal: 'send data timeout; data not sent'].
		count := self primSocket: socketHandle
			sendUDPData: aStringOrByteArray
			toHost: hostAddress
			port: portNumber
			startIndex: bytesSent + 1
			count: bytesToSend - bytesSent.
		count = 0 ifTrue: 
			["Usually broadcast fails in primitive without the proper option set, 
			but some platforms simply return count=0"
			(self broadcastMisconfiguredForSendingTo: hostAddress)
				ifTrue: [ ^ self broadcastError: hostAddress ].
			bytesToSend ~= count 
				ifTrue: [ ^NetworkError signal: 'failed to send data']].
		bytesSent := bytesSent + count.
		bytesSent < bytesToSend] whileTrue.

	^ bytesSent
