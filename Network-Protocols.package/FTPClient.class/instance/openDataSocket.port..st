protocol
openDataSocket: remoteHostAddress port: dataPort
	dataSocket := Socket new.
	dataSocket connectTo: remoteHostAddress port: dataPort