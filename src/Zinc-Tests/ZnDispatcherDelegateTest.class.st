Class {
	#name : #ZnDispatcherDelegateTest,
	#superclass : #TestCase,
	#category : #'Zinc-Tests'
}

{ #category : #testing }
ZnDispatcherDelegateTest >> testDispatcherDelegate [
	self
		withServerDo: [ :server | 
			| client |
			server
				delegate:
					(ZnDispatcherDelegate new
						map: '/hello' to: [ :request :response | response entity: (ZnStringEntity html: '<h1>hello server</h1>') ];
						map: '/' to: [ :request :response | response entity: (ZnStringEntity html: '<h1>default</h1>') ]).

			client := ZnClient new.

			client
				get:
					(server localUrl
						addPathSegment: 'missing';
						yourself).
			self assert: client response contentType equals: ZnMimeType textPlain.
			self assert: client response status equals: 404.

			client get: server localUrl.
			self assert: client response contentType equals: ZnMimeType textHtml.
			self assert: client response status equals: 200.

			client
				get:
					(server localUrl
						addPathSegment: 'hello';
						yourself).
			self assert: client response contentType equals: ZnMimeType textHtml.
			self assert: client response status equals: 200.

			client close ]
]

{ #category : #private }
ZnDispatcherDelegateTest >> withServerDo: block [
	| server |
	server := ZnServer on: 1700 + 32 atRandom.
	[ 
		server start.
		self 
			assert: server isRunning & server isListening
			description: ('Failed to start server on port {1}. Is there one already?' format: { server port }).
		self assert: server isRunning & server isListening.
		block cull: server
	] 
		ensure: [ server stop ]
]
