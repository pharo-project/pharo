private protocol
lookForCode: code
	"We are expecting a certain code next."

	self
		lookForCode: code
		ifDifferent: [:response | (TelnetProtocolError protocolInstance: self) signal: response]
