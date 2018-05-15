"
This class implements POP3 (Post Office Protocol 3) as specified in RFC 1939.  (see http://www.ietf.org/rfc.html)

You can use it to download email from the mail server to your personal mail program.

To see an example of it's use, see POPSocket class>>example.
"
Class {
	#name : #POP3Client,
	#superclass : #ProtocolClient,
	#category : #'Network-Protocols-Protocols'
}

{ #category : #accessing }
POP3Client class >> defaultPortNumber [
	^110
]

{ #category : #example }
POP3Client class >> example [

	"POP3Client example"
	"download a user's messages into an OrderedCollection and inspect the OrderedCollection"

	| ps messages userName password |
	userName := (UIManager default request: 'POP username').
	password := (UIManager default request: 'POP password').
	ps := POP3Client openOnHostNamed: (UIManager default request: 'POP server').
	[
	ps loginUser: userName password: password.
	ps logProgressToTranscript.

	messages := OrderedCollection new.
	1 to: ps messageCount do: [ :messageNr |
		messages add: (ps retrieveMessage: messageNr) ]]
		ensure: [ps close].

	messages inspect.
]

{ #category : #accessing }
POP3Client class >> logFlag [
	^#pop
]

{ #category : #'private protocol' }
POP3Client >> apopLogin [
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
]

{ #category : #'public protocol' }
POP3Client >> apopLoginUser: userName password: password [

	self loginUser: userName password: password loginMethod: #APOP
]

{ #category : #'private protocol' }
POP3Client >> clearTextLogin [

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
]

{ #category : #'public protocol' }
POP3Client >> deleteMessage: num [
	"delete the numbered message"

	self ensureConnection.
	self sendCommand: 'DELE ', num printString.
	self checkResponse.
	self logProgress: self lastResponse
]

{ #category : #'private protocol' }
POP3Client >> getMultilineResponse [
	"Get a multiple line response to the last command, filtering out LF characters. A multiple line response ends with a line containing only a single period (.) character."

	| response done chunk |
	response := String new writeStream.
	done := false.
	[done] whileFalse: [
		chunk := self stream nextLine.
		(chunk beginsWith: '.')
			ifTrue: [response nextPutAll: (chunk copyFrom: 2 to: chunk size); cr ]
			ifFalse: [response nextPutAll: chunk; cr ].
		done := (chunk = '.') ].

	^ response contents

]

{ #category : #'private protocol' }
POP3Client >> login [
	self loginMethod
		ifNil: [^self].
	self loginMethod == #clearText
		ifTrue: [^self clearTextLogin].
	self loginMethod == #APOP
		ifTrue: [^self apopLogin].
	(POP3LoginError protocolInstance: self) signal: 'Unsupported login procedure.'
]

{ #category : #private }
POP3Client >> loginMethod [
	^self connectionInfo at: #loginMethod ifAbsent: [nil]
]

{ #category : #private }
POP3Client >> loginMethod: aSymbol [
	^self connectionInfo at: #loginMethod put: aSymbol
]

{ #category : #'public protocol' }
POP3Client >> loginUser: userName password: password [

	self loginUser: userName password: password loginMethod: #clearText
]

{ #category : #'public protocol' }
POP3Client >> loginUser: userName password: password loginMethod: aLoginMethod [

	self user: userName.
	self password: password.
	self loginMethod: aLoginMethod.
	self login
]

{ #category : #'public protocol' }
POP3Client >> messageCount [
	"Query the server and answer the number of messages that are in the user's mailbox."

	| numMessages |
	self ensureConnection.
	self sendCommand: 'STAT'.
	self checkResponse.
	self logProgress: self lastResponse.

	[ | answerString |answerString := (self lastResponse findTokens: Character separators) second.
	numMessages := answerString asNumber asInteger]
		on: Error
		do: [:ex | (ProtocolClientError protocolInstance: self) signal: 'Invalid STAT response.'].
	^numMessages
]

{ #category : #'public protocol' }
POP3Client >> quit [
	"QUIT <CRLF>"

	self sendCommand: 'QUIT'.
	self checkResponse.
]

{ #category : #'private testing' }
POP3Client >> responseIsError [
	^self lastResponse beginsWith: '-'
]

{ #category : #'private testing' }
POP3Client >> responseIsWarning [
	^self lastResponse beginsWith: '-'
]

{ #category : #'public protocol' }
POP3Client >> retrieveMessage: number [
	"retrieve the numbered message"

	self ensureConnection.
	self sendCommand: 'RETR ', number printString.
	self checkResponse.
	self logProgress: self lastResponse.

	^self getMultilineResponse
]
