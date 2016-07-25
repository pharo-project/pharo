testing
testGetAfterPost
	self withServerDo: [ :server | | client |
		server onRequestRespond: [ :request | 
			request uri firstPathSegment = 'one'
				ifTrue: [
					(request method = #POST and: [ request hasEntity ])
						ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK for one') ]
						ifFalse: [ ZnResponse badRequest: request ] ]
				ifFalse: [
					request uri firstPathSegment = 'two'
						ifTrue: [ 
							(request method = #GET and: [ request hasEntity not ])
								ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK for two') ]
								ifFalse: [ ZnResponse badRequest: request ] ]
		 				ifFalse: [ ZnResponse notFound: request uri ] ] ].
		(client := ZnClient new) 
			autoResetEntityMethods: #(HEAD DELETE GET);
			url: server localUrl;
			url: 'one';
			entity: (ZnEntity text: 'One two three');
			post.
		self assert: client isSuccess.
		self assert: client contents = 'OK for one'.
		client
			url: 'two';
			get.
		self assert: client isSuccess.
		self assert: client contents = 'OK for two'.
		client close ]