private
withUDPEchoServer: block
	"Run a local UDP echo server on localhost:port and execute block.
	Optionally pass the new process to block. When quit is received, stop the server"
	
	| socket loop result process |
	socket := Socket newUDP setPort: self port; yourself.
	loop := true.
	process := [ 
		[ [ loop ] whileTrue: [ 
			result := self receiveUDPDataFrom: socket.
			"echo what we received to where it came from"
			socket sendUDPData: result first toHost: result second port: result third.
			result first asLowercase = #quit ifTrue: [ loop := false ] ] ]
				ensure: [ socket closeAndDestroy ] ] 
					forkAt: Processor userBackgroundPriority named: 'UDP echo server'.
	^ block cull: process