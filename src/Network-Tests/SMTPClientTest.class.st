"
SUnit tests for SMTP client
"
Class {
	#name : #SMTPClientTest,
	#superclass : #TestCase,
	#instVars : [
		'smtp',
		'socket'
	],
	#category : #'Network-Tests-Protocols'
}

{ #category : #running }
SMTPClientTest >> setUp [
	super setUp.
	socket := MockSocketStream on: ''.
	smtp := SMTPClient new.
	smtp stream: socket.
]

{ #category : #testing }
SMTPClientTest >> testMailFrom [
	smtp mailFrom: 'frank@angband.za.org'.
	self assert: socket outStream contents equals: 'MAIL FROM: <frank@angband.za.org>', String crlf.
	
	socket resetOutStream.
	smtp mailFrom: '<frank@angband.za.org>'.
	self assert: socket outStream contents equals: 'MAIL FROM: <frank@angband.za.org>', String crlf.
	
	socket resetOutStream.
	smtp mailFrom: 'Frank <frank@angband.za.org>'.
	self assert: socket outStream contents equals: 'MAIL FROM: <frank@angband.za.org>', String crlf
]
