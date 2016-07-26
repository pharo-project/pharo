testing
testEchoLocalInterface
	| server response |
	(server := ZnServer on: self port)
		bindingAddress: NetNameResolver loopBackAddress.
	[ 
		server start.
		self 
			assert: server isRunning & server isListening
			description: ('Failed to start server on port {1}. Is there one already?' format: { server port }).
		response := ZnEasy get: (server localUrl addPathSegments: #('echo' 'foo'); yourself).
		self assert: (response contentType = ZnMimeType textPlain).
		self assert: (response statusLine code = 200).
		self assert: (response entity contents includesSubstring: 'Zinc').
		self assert: (response entity contents includesSubstring: 'foo')
	]
		ensure: [ server stop ]