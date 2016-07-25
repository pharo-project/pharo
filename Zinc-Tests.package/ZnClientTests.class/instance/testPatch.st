testing
testPatch
	| client |
	self withServerDo: [ :server |
		server onRequestRespond: [ :request | 
			request method = #PATCH
				ifTrue: [| response |
					response := ZnResponse noContent.
					response headers at: #Etag put: '"e0023aa4f"'.
					response]
				ifFalse: [ ZnResponse badRequest: request ] ].
		(client := ZnClient new)
			patch: server localUrl contents: 'Some data as text'.
		self assert: client isSuccess.
		self deny: client response hasEntity.
		self assert: (client response headers at: #Etag) equals: '"e0023aa4f"'.
		client close ]