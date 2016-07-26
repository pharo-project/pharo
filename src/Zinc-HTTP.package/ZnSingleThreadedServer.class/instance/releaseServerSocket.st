private
releaseServerSocket
	"Release our server socket"
	
	| address port |
	serverSocket ifNil: [ ^ self ].
	address := [ serverSocket address ] on: Error do: [ nil ].
	port := [ serverSocket port ]on: Error do: [ nil ].
	(Delay forMilliseconds: 10) wait.
	serverSocket destroy.
	serverSocket := nil.
	self logServerSocketReleasedAddress: address port: port 