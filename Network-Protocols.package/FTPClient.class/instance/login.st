private
login

	self user ifNil: [^self].

	["repeat both USER and PASS since some servers require it"
	self sendCommand: 'USER ', self user.

	"331 Password required"
	self lookForCode: 331.
	"will ask user, if needed"
	self sendCommand: 'PASS ', self password.

	"230 User logged in"
	([self lookForCode: 230.]
		on: TelnetProtocolError
		do: [false]) == false
		] whileTrue: [
			(LoginFailedException protocolInstance: self) signal: self lastResponse]

