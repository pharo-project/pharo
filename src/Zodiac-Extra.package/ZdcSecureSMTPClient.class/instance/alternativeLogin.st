private protocol
alternativeLogin
	self initiateSession. "send EHLO first"
	self user ifNil: [ ^ self ].
	self sendCommand: 'AUTH LOGIN'.
	self checkResponse.
	self sendCommand: (self encodeString: self user).
	self checkResponse.
	self sendCommand: (self encodeString: self password).
	self checkResponse