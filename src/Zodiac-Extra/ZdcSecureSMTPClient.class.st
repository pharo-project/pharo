"
I am ZdcSecureSMTPClient.

I open a ZdcSecureSocketStream to the SMTP server and connect it at the SSL level.

| mailMessage |
mailMessage := MailMessage empty.
mailMessage setField: 'subject' toString: 'ZdcSecureSMTPClient Test'.
mailMessage body: (MIMEDocument contentType: 'text/plain' content: 'This is test from Pharo Smalltalk').
ZdcSecureSMTPClient
	sendUsingGMailAccount: '<your-name>@gmail.com' 
	password: '<your-password>'
	to: '<email-address>' 
	message: mailMessage
"
Class {
	#name : #ZdcSecureSMTPClient,
	#superclass : #SMTPClient,
	#instVars : [
		'variant',
		'lineReader'
	],
	#category : #'Zodiac-Extra'
}

{ #category : #constants }
ZdcSecureSMTPClient class >> defaultPortForSSL [
	^ 465
]

{ #category : #constants }
ZdcSecureSMTPClient class >> defaultPortForStartTLS [
	^ 587
]

{ #category : #example }
ZdcSecureSMTPClient class >> sendUsingGMailAccount: senderAddress password: password to: receiverAddress message: mailMessage [
	| smtpClient |
	(smtpClient := self new)
		user: senderAddress; 
		password: password.
	smtpClient openOnHost: (NetNameResolver addressForName: 'smtp.gmail.com') port: 465.
	mailMessage setField: 'from' toString: senderAddress.
	mailMessage setField: 'to' toString: receiverAddress.
	^ smtpClient 
		mailFrom: senderAddress to: { receiverAddress } text: mailMessage text;
		quit; 
		close;
		yourself

]

{ #category : #'private protocol' }
ZdcSecureSMTPClient >> alternativeLogin [
	self initiateSession. "send EHLO first"
	self user ifNil: [ ^ self ].
	self sendCommand: 'AUTH LOGIN'.
	self checkResponse.
	self sendCommand: (self encodeString: self user).
	self checkResponse.
	self sendCommand: (self encodeString: self password).
	self checkResponse
]

{ #category : #private }
ZdcSecureSMTPClient >> ensureConnection [
	"Overwritten & refactored"
		
	self isConnected ifTrue: [ ^ self].
	self stream ifNotNil: [ self stream close ].
	self setupStream.
	"regarding RFC2487-5.2 the state after SSL negotiation is to be reset to the state after the
	220 server greeting. EHLO command is next which is issued in the following login method  "
	self login
]

{ #category : #'private protocol' }
ZdcSecureSMTPClient >> fetchNextResponse [
	"The FTP and similar protocols allow multi-line responses.
	If the response is multi-line, the fourth character of the first line is a  
	$- and the last line repeats the numeric code but the code is followed by 
	a space."

	| response result firstLine |
	result := '' writeStream.
	firstLine := self nextLineFromStream.
	result nextPutAll: firstLine.
	(self responseIsContinuation: firstLine) 
		ifTrue: [
			"continued over multiple lines. Discard continuation lines."
			[response := self nextLineFromStream.
			response ifNil: [ ^ nil ].
			response size > 3 and: 
					[ (response copyFrom: 1 to: 3) = (firstLine copyFrom: 1 to: 3) 
						and: [ (response at: 4) = Character space ] ] ] whileFalse: [
							result cr; nextPutAll: response ] ].
	self lastResponse: result contents
]

{ #category : #initialization }
ZdcSecureSMTPClient >> initialize [
	super initialize.
	self useSSL
]

{ #category : #testing }
ZdcSecureSMTPClient >> isSSL [
	^ variant = #ssl
]

{ #category : #testing }
ZdcSecureSMTPClient >> isStartTLS [
	^ variant = #startTLS
]

{ #category : #private }
ZdcSecureSMTPClient >> logVerbose: aString [
	"self log: aString; log: Character cr"
]

{ #category : #'private protocol' }
ZdcSecureSMTPClient >> nextLineFromStream [
	| line |
	line := lineReader nextLine.
	self logVerbose: line.
	^ line
]

{ #category : #'private protocol' }
ZdcSecureSMTPClient >> requireStartTLS [
	(self lastResponse includesSubstring: 'STARTTLS')
		ifFalse: [ ^ self error: 'Server does not seem to support STARTTLS' ].

]

{ #category : #'private protocol' }
ZdcSecureSMTPClient >> sendCommand: aString [
	self logVerbose: aString.
	self stream 
		nextPutAll: aString;
		nextPutAll: #[13 10];
		flush

]

{ #category : #private }
ZdcSecureSMTPClient >> setupStream [
	variant = #ssl ifTrue: [ ^ self setupStreamForSSL ].
	variant = #startTLS ifTrue: [ ^ self setupStreamForStartTLS ]
]

{ #category : #private }
ZdcSecureSMTPClient >> setupStreamForSSL [
	self stream: (ZdcSecureSocketStream openConnectionToHost: self host port: self port).
	self stream connect.
	self logVerbose: 'SSL Connect OK.'.
	"as we connect directly with SSL the first message will be a 220 server greeting"
	self checkResponse.

]

{ #category : #private }
ZdcSecureSMTPClient >> setupStreamForStartTLS [
	"See http://www.ietf.org/rfc/rfc3207.txt for how to setup a secure connection"

	self stream: (SocketStream openConnectionToHost: self host port: self port).
	self checkResponse.
	self initiateSession.
	self requireStartTLS.
	self startTLS
]

{ #category : #'private protocol' }
ZdcSecureSMTPClient >> startTLS [
	"Send a STARTTLS command and after the response, 
	switch to a SecureSocketStream and do the client handshake"
	
	self sendCommand: 'STARTTLS'.
	self checkResponse.	
	self stream: (ZdcSecureSocketStream on: self stream socket).
	self stream connect.
	self logVerbose: 'SSL Connect OK.'
]

{ #category : #accessing }
ZdcSecureSMTPClient >> stream: aStream [
	super stream: aStream.
	lineReader := ZnLineReader on: self stream
]

{ #category : #'initialize-release' }
ZdcSecureSMTPClient >> useSSL [
	variant := #ssl
]

{ #category : #'initialize-release' }
ZdcSecureSMTPClient >> useStartTLS [
	variant := #startTLS
]
