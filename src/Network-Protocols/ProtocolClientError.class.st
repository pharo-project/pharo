"
Abstract super class for protocol clients

	protocolInstance		reference to the protocol client throughing the exception. Exception handlers can access the client in order close, respond or whatever may be appropriate

"
Class {
	#name : #ProtocolClientError,
	#superclass : #Error,
	#instVars : [
		'protocolInstance'
	],
	#category : #'Network-Protocols'
}

{ #category : #'instance creation' }
ProtocolClientError class >> protocolInstance: aProtocolInstance [
	^self new protocolInstance: aProtocolInstance
]

{ #category : #accessing }
ProtocolClientError >> messageText [
	^super messageText
		ifNil: [self response]
]

{ #category : #accessing }
ProtocolClientError >> protocolInstance [
	^protocolInstance
]

{ #category : #accessing }
ProtocolClientError >> protocolInstance: aProtocolInstance [
	protocolInstance := aProtocolInstance
]

{ #category : #accessing }
ProtocolClientError >> response [
	^self protocolInstance lastResponse
]
