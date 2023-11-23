"
Abstract super class for exceptions signalled by clients based on the telnet protocol.

"
Class {
	#name : 'TelnetProtocolError',
	#superclass : 'ProtocolClientError',
	#category : 'Network-Protocols-Exceptions',
	#package : 'Network-Protocols',
	#tag : 'Exceptions'
}

{ #category : 'accessing' }
TelnetProtocolError >> code [
	^self protocolInstance responseCode
]

{ #category : 'private' }
TelnetProtocolError >> isCommandUnrecognized [
	^ self code = 500
]
