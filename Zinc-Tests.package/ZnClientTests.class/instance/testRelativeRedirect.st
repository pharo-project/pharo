testing
testRelativeRedirect
	self withServerDo: [ :server | | client |
		server onRequestRespond: [ :request | 
			request uri firstPathSegment = 'one'
				ifTrue: [ ZnResponse redirect: 'two' ]
				ifFalse: [
					request uri firstPathSegment = 'two'
						ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK!') ]
		 				ifFalse: [ ZnResponse notFound: request uri ] ] ].
		(client := ZnClient new) 
			url: server localUrl; addPath: 'one'; 
			post.
		self assert: client isSuccess.
		self assert: client contents = 'OK!'.
		client close ]