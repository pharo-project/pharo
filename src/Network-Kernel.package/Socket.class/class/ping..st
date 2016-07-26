utilities
ping: hostName
	"Ping the given host. Useful for checking network connectivity. The host must be running a TCP echo server."

	"Socket ping: 'pharo-project.org'"

	| tcpPort sock serverAddr startTime echoTime |
	tcpPort := 7.	"7 = echo port, 13 = time port, 19 = character generator port"
	serverAddr := NetNameResolver addressForName: hostName timeout: 10.
	serverAddr 
		ifNil: [ ^ self inform: 'Could not find an address for ' , hostName ].
	sock := Socket new.
	sock connectNonBlockingTo: serverAddr port: tcpPort.
	[ sock waitForConnectionFor: 10 ]
		on: ConnectionTimedOut
		do: [ :ex | 
			(self confirm: 'Continue to wait for connection to ' , hostName , '?')
				ifTrue: [ ex retry ]
				ifFalse: [ 
					sock destroy.
					^ self ] ].
	sock sendData: 'echo!'.
	startTime := Time millisecondClockValue.
	[ sock waitForDataFor: 15 ]
		on: ConnectionTimedOut
		do: [ :ex | 
			(self confirm: 'Packet sent but no echo yet; keep waiting?')
				ifTrue: [ ex retry ] ].
	echoTime := Time millisecondsSince: startTime.
	sock destroy.
	self inform: hostName , ' responded in ' , echoTime printString , ' milliseconds'