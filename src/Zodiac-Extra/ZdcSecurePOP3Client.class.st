"
I am ZdcSecurePOP3Client.

I open a ZdcSecureSocketStream to the POP3 server and connect it at the SSL level.

ZdcSecurePOP3Client
	retrieveMessagesFromGMailAccount: '<your-name>@gmail.com'
	password: '<your-password>'
	limit: 5.
"
Class {
	#name : #ZdcSecurePOP3Client,
	#superclass : #POP3Client,
	#instVars : [
		'lineReader'
	],
	#category : #'Zodiac-Extra'
}

{ #category : #constants }
ZdcSecurePOP3Client class >> defaultPortForSSL [
	^ 995
]

{ #category : #example }
ZdcSecurePOP3Client class >> retrieveMessagesFromGMailAccount: accountAddress password: password limit: limit [
	| pop3Client messages messageCount |
	(pop3Client := self openOnHostNamed: 'smtp.gmail.com' port: self defaultPortForSSL)
		loginUser: accountAddress password: password.
	messages := OrderedCollection new.
	messageCount := pop3Client messageCount.
	messageCount to: ((messageCount - limit) max: 0) + 1 by: -1 do: [ :each |
		messages add: (pop3Client retrieveMessage: each) ].
	pop3Client
		quit;
		close.
	^ messages

]

{ #category : #private }
ZdcSecurePOP3Client >> ensureConnection [
	"Overwritten & refactored"
		
	self isConnected ifTrue: [ ^ self ].
	self stream ifNotNil: [ self stream close ].

	self setupStream.
	self checkResponse.
	self login
]

{ #category : #'private protocol' }
ZdcSecurePOP3Client >> fetchNextResponse [
	self lastResponse: self nextLineFromStream
]

{ #category : #'private protocol' }
ZdcSecurePOP3Client >> getMultilineResponse [
	"Get a multiple line response to the last command, filtering out LF characters. 
	A multiple line response ends with a line containing only a single period (.) character."

	| response done chunk |
	response := String new writeStream.
	done := false.
	[ done ] whileFalse: [
		chunk := self nextLineFromStream.
		(chunk beginsWith: '.')
			ifTrue: [ response nextPutAll: (chunk copyFrom: 2 to: chunk size); cr ]
			ifFalse: [ response nextPutAll: chunk; cr ].
		done := (chunk = '.') ].
	^ response contents

]

{ #category : #private }
ZdcSecurePOP3Client >> logVerbose: aString [
	"self log: aString; log: Character cr"
]

{ #category : #'private protocol' }
ZdcSecurePOP3Client >> nextLineFromStream [
	| line |
	line := lineReader nextLine.
	self logVerbose: line.
	^ line
]

{ #category : #'private protocol' }
ZdcSecurePOP3Client >> sendCommand: aString [
	self logVerbose: aString.
	self stream 
		nextPutAll: aString;
		nextPutAll: #[13 10];
		flush

]

{ #category : #private }
ZdcSecurePOP3Client >> setupStream [
	self setupStreamForSSL
]

{ #category : #private }
ZdcSecurePOP3Client >> setupStreamForSSL [
	self stream: (ZdcSecureSocketStream openConnectionToHost: self host port: self port).
	self stream connect.
	self logVerbose: 'SSL Connect OK.'
]

{ #category : #accessing }
ZdcSecurePOP3Client >> stream: aStream [
	super stream: aStream.
	lineReader := ZnLineReader on: self stream
]
