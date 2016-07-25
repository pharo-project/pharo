private protocol
clearTextLogin

	[self sendCommand: 'USER ', self user.
	self checkResponse.
	self logProgress: self lastResponse.

	self sendCommand: 'PASS ', self password.
	self checkResponse.
	self logProgress: self lastResponse]
		on: TelnetProtocolError
		do: [:ex |
			"Neither authentication worked.  Indicate an error and close up"
			self close.
			ex resignalAs: ((LoginFailedException protocolInstance: self) signal: 'Login failed.')]