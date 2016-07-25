enumerating
clientDo: aBlock
	| client |
	client := FTPClient openOnHostNamed: host.
	client loginUser: user password: password.
	directory isEmpty ifFalse: [client changeDirectoryTo: directory].
	^ [aBlock value: client] ensure: [client close]