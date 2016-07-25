testing
testDispatcherDelegate
	self withServerDo: [ :server |  | client |
		server delegate: (ZnDispatcherDelegate new 
			map: '/hello' to: [ :request :response | response entity: (ZnStringEntity html: '<h1>hello server</h1>') ];
			map: '/' to: [ :request :response | response entity: (ZnStringEntity html: '<h1>default</h1>') ]).

		client := ZnClient new.
		
		client get: (server localUrl addPathSegment: 'missing'; yourself).
		self assert: (client response contentType = ZnMimeType textPlain).
		self assert: (client response status = 404).
		
		client get: server localUrl.
		self assert: (client response contentType = ZnMimeType textHtml).
		self assert: (client response status = 200).
		
		client get: (server localUrl addPathSegment: 'hello'; yourself).
		self assert: (client response contentType = ZnMimeType textHtml).
		self assert: (client response status = 200).
		
		client close ]