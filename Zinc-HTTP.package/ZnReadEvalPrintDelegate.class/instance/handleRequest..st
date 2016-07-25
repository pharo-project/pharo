public
handleRequest: request
	request uri firstPathSegment = #repl
		ifTrue: [ 
			request method = #GET
				ifTrue: [ 
					^ self handleGetRequest: request ].
			(request method = #POST and: [ request hasEntity and: [ request contentType isBinary not ] ])
				ifTrue: [ 
					^ self handlePostRequest: request ] ].
	^ ZnResponse notFound: request uri