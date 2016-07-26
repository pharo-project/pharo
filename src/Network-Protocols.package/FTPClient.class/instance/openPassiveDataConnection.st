private protocol
openPassiveDataConnection
	| portInfo list dataPort remoteHostAddress remoteAddressString |
	self sendCommand: 'PASV'.
	self lookForCode: 227 ifDifferent: [:response | (TelnetProtocolError protocolInstance: self) signal: 'Could not enter passive mode: ' , response].

	portInfo := (self lastResponse findTokens: '()') at: 2.
	list := portInfo findTokens: ','.
	remoteHostAddress := ByteArray
		with: (list at: 1) asNumber
		with: (list at: 2) asNumber
		with: (list at: 3) asNumber
		with: (list at: 4) asNumber.
	remoteAddressString := String streamContents: [:addrStream | remoteHostAddress
		do: [ :each | each printOn: addrStream ]
		separatedBy: [ addrStream nextPut: $. ]].
 	dataPort := (list at: 5) asNumber * 256 + (list at: 6) asNumber.
	self openDataSocket: (NetNameResolver addressForName: remoteAddressString) port: dataPort