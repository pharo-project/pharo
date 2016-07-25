private protocol
login
	self initiateSession. "send EHLO first"
	self user ifNil: [^self].
	self sendCommand: 'AUTH LOGIN ' , (self encodeString: self user).
	[self checkResponse]
		on: TelnetProtocolError
		do: [ :ex | ex isCommandUnrecognized ifTrue: [^ self] ifFalse: [ex pass]].
	self sendCommand: (self encodeString: self password).
	self checkResponse