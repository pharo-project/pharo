"
I am ZnDispatcherDelegate, doing a straight-forward dispatching to mapped URLs.

ZnDispatcherDelegate API:

(server := ZnServer startDefaultOn: 9090)
	delegate: (ZnDispatcherDelegate new 
		map: '/hello' to: [ :request :response | response entity: (ZnEntity html: '<h1>hello server</h1>') ];
		map: '/counter' to: [ :request :response | counterApplication handleRequest: request response: response ]).
"
Class {
	#name : #ZnDispatcherDelegate,
	#superclass : #Object,
	#instVars : [
		'dispatcherDictionary'
	],
	#category : #'Zinc-HTTP-Client-Server'
}

{ #category : #accessing }
ZnDispatcherDelegate >> dispatcherDictionary [
	^ dispatcherDictionary ifNil: [ dispatcherDictionary := Dictionary new ]
]

{ #category : #public }
ZnDispatcherDelegate >> handleRequest: request [
	| handler |
	handler := self dispatcherDictionary 
		at: request uri pathPrintString 
		ifAbsent: [ ^ ZnResponse notFound: request uri ].	
	^ handler 
		value: request 
		value: (ZnResponse new
			statusLine: (ZnStatusLine ok);
			headers: ZnHeaders defaultResponseHeaders;
			yourself).


]

{ #category : #public }
ZnDispatcherDelegate >> map: relativeUrlString to: handler [
	self dispatcherDictionary at: relativeUrlString put: handler
]
