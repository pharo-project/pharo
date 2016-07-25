private protocol
apopLogin
	"Attempt to authenticate ourselves to the server without sending the password as cleartext."

	"For secure authentication, we look for a timestamp in the initial response string we get from the server, and then try the APOP command as specified in RFC 1939.  If the initial response from the server is
	+OK POP3 server ready <1896.697170952@dbc.mtview.ca.us>
we extract the timestamp
	<1896.697170952@dbc.mtview.ca.us>
then form a string of the form
	<1896.697170952@dbc.mtview.ca.us>USERPASSWORD
and then send only the MD5 hash of that to the server.  Thus the password never hits the wire"

	[ 
	| hash timestamp |
	"Look for a timestamp in the response we received from the server"
	timestamp := self lastResponse findTokens: '<>' includes: '@'.
	timestamp ifNil: [ (POP3LoginError protocolInstance: self) signal: 'APOP not supported.' ].
	(Smalltalk globals includesKey: #MD5)
		ifTrue: [ 
			hash := ((Smalltalk globals at: #MD5) hashMessage: '<' , timestamp , '>' , self password) storeStringHex asLowercase.	"trim starting 16r and zero pad it to 32 characters if needed"
			hash := hash padLeftTo: 32 with: $0 ]
		ifFalse: [ (POP3LoginError protocolInstance: self) signal: 'APOP (MD5) not supported.' ].
	self sendCommand: 'APOP ' , self user , ' ' , hash.
	self checkResponse.
	self logProgress: self lastResponse ]
		on: ProtocolClientError
		do: [ :ex | 
			self close.
			(LoginFailedException protocolInstance: self) signal: 'Login failed.' ]