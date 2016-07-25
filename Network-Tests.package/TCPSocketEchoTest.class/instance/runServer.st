testing
runServer
	"Run and return a local TCP echo server"
	
	^ self withTCPEchoServer: [ :process | process ]