initialize-release
connectTo: hostAddress port: portNumber
	"Connect our socket to hostAddress:portNumber.
	Wait up to timeout for a connection"
	
	self socketConnectTo: hostAddress port: portNumber