private protocol
checkResponse
	self
		checkResponseOnError: [:response | (TelnetProtocolError protocolInstance: self)
				signal: response]
		onWarning: [:response | (TelnetProtocolError protocolInstance: self)
				signal: response]