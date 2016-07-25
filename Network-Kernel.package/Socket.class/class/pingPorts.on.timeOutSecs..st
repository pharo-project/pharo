utilities
pingPorts: portList on: hostName timeOutSecs: timeOutSecs
	"Attempt to connect to each of the given sockets on the given host. Wait at most timeOutSecs for the connections to be established. Answer an array of strings indicating the available ports."

	"Socket pingPorts: #(7 13 19 21 23 25 80 110 119) on: 'pharo-project.org' timeOutSecs: 15"

	| serverAddr sockets startTime timeoutMsecs done result unconnectedCount connectedCount waitingCount |
	serverAddr := NetNameResolver addressForName: hostName timeout: 10.
	serverAddr 
		ifNil: [ 
			self inform: 'Could not find an address for ' , hostName.
			^ #() ].
	sockets := portList
		collect: [ :portNum | 
			| sock |
			sock := Socket new.
			sock connectTo: serverAddr port: portNum ].
	startTime := Time millisecondClockValue.
	timeoutMsecs := (1000 * timeOutSecs) truncated.
	done := false.
	[ done ]
		whileFalse: [ 
			unconnectedCount := 0.
			connectedCount := 0.
			waitingCount := 0.
			sockets
				do: [ :s | 
					s isUnconnectedOrInvalid
						ifTrue: [ unconnectedCount := unconnectedCount + 1 ]
						ifFalse: [ 
							s isConnected
								ifTrue: [ connectedCount := connectedCount + 1 ].
							s isWaitingForConnection
								ifTrue: [ waitingCount := waitingCount + 1 ] ] ].
			waitingCount = 0
				ifTrue: [ done := true ].
			connectedCount = sockets size
				ifTrue: [ done := true ].
			(Time millisecondsSince: startTime) >= timeoutMsecs
				ifTrue: [ done := true ] ].
	result := (sockets select: [ :s | s isConnected ]) collect: [ :s | self nameForWellKnownTCPPort: s remotePort ].
	sockets do: [ :s | s destroy ].
	^ result