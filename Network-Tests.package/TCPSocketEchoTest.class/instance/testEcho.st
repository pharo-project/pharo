testing
testEcho
	| socket message result |
	socket := Socket newTCP.
	[ 
		self withTCPEchoServer: [ :process |
			message := 'Testing ', 99 atRandom asString. 
			socket connectTo: self localhost port: self port.
			socket sendData: message.
			result := socket receiveData.
			self assert: result equals: message ]  
	] ensure: [ socket closeAndDestroy ].
	"At this point the server is still running, ask it to kill itself"
	self clientSend: #quit