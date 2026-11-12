Class {
	#name : 'ZnProxyServerDelegate',
	#superclass : 'Object',
	#instVars : [
		'proxyBlock',
		'server'
	],
	#category : 'Zinc-HTTP-Examples',
	#package : 'Zinc-HTTP-Examples'
}

{ #category : 'private' }
ZnProxyServerDelegate >> augmentUrl: aUrl [
	"The URL in a server request has no scheme, host or port set.
	Augment aUrl by taking those elements from #serverUrl"

	self server ifNil: [ ^ aUrl ].
	^ aUrl inContextOf: self server url
]

{ #category : 'private' }
ZnProxyServerDelegate >> copyAndPrepareRequest: aRequest [
	"We make a copy of the request and augment its URL"

	| copiedRequest |
	copiedRequest := aRequest copy.
	copiedRequest url: (self augmentUrl: copiedRequest url).
	^ copiedRequest
]

{ #category : 'public' }
ZnProxyServerDelegate >> handleRequest: incomingRequest [
	"Ask our proxy block to transform a incoming ZnRequest copy into an outgoing ZnRequest"

	| outgoingRequest |
	outgoingRequest := self proxyBlock 
		cull: (self copyAndPrepareRequest: incomingRequest) 
		cull: self server.
	^ ZnClient new
		request: outgoingRequest;
		beOneShot;
		execute;
		response
]

{ #category : 'accessing' }
ZnProxyServerDelegate >> proxyBlock [
	^ proxyBlock
]

{ #category : 'accessing' }
ZnProxyServerDelegate >> proxyBlock: aBlock [
	"Set the block that transforms a copy of the incoming ZnRequest to the outgoing ZnRequest.
	Two parameters are passed to block: the request and an optional server reference.
	See #copyAndPrepareRequest: for how the request got changed."

	proxyBlock := aBlock
]

{ #category : 'accessing' }
ZnProxyServerDelegate >> server [
	^ server
]

{ #category : 'accessing' }
ZnProxyServerDelegate >> server: aZnServer [
	"Set areference to the server we're in (optional)"

	server := aZnServer
]
