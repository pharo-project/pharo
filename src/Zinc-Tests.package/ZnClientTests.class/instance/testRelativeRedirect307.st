testing
testRelativeRedirect307
	self withServerDo: [ :server | | client |
		server onRequestRespond: [ :request | 
			request uri firstPathSegment = 'one'
				ifTrue: [ (ZnResponse redirect: 'two') statusLine: (ZnStatusLine code: 307); yourself ]
				ifFalse: [
					((request uri firstPathSegment = 'two') 
							and: [ request method = #POST and: [ request contents = 'BODY' ] ])
						ifTrue: [ ZnResponse ok: (ZnEntity text: 'OK!') ]
		 				ifFalse: [ ZnResponse notFound: request uri ] ] ].
		(client := ZnClient new) 
			url: server localUrl; addPath: 'one'; 
			entity: (ZnEntity text: 'BODY');
			post.
		self assert: client isSuccess.
		self assert: client contents = 'OK!'.
		client close ]