testing
testRedirectWithCustomHeader
	self withServerDo: [ :server | | client cookie |
		server onRequestRespond: [ :request | 
			(request headers at: 'X-Custom' ifAbsent: [ nil ]) = 'The Secret'
				ifFalse: [ ZnResponse badRequest: request ]
				ifTrue: [
					request uri firstPathSegment = 'one'
						ifTrue: [ ZnResponse redirect: 'two' ]
						ifFalse: [
							request uri firstPathSegment = 'two'
								ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK!') ]
		 						ifFalse: [ ZnResponse badRequest: request ] ] ] ].
		(client := ZnClient new) 
			url: server localUrl; 
			addPath: 'one';
			headerAt: 'X-Custom' put: 'The Secret'; 
			get.
		self assert: client isSuccess.
		self assert: client contents = 'OK!'.
		client close ]