testing
runServer
	"Run and return a local UDP echo server"
	
	^ self withUDPEchoServer: [ :process | process ]