testing
testOptions
	| client |
	self withServerDo: [ :server |
		server onRequestRespond: [ :request | 
			request method = #OPTIONS
				ifTrue: [ | response |
					response := ZnResponse noContent.
					response headers at: #Allow put: 'GET, HEAD'.
					response ]
				ifFalse: [ ZnResponse badRequest: request ] ].
		(client := ZnClient new)
			options: server localUrl.
		self assert: client isSuccess.
		self deny: client response hasEntity.
		self assert: (client response headers at: #Allow) equals: 'GET, HEAD'.
		client close ]