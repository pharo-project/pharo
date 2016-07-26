public
handleRequest: request
	(request method = #GET)
		ifTrue: [
			^ request uri isSlash
				ifTrue: [ self handleListMczEntries: request ]
				ifFalse: [ self handleGetMczEntry: request ] ].
	^ (request method = #PUT) 
		ifTrue: [ self handlePutMczEntry: request ]
		ifFalse: [ ZnResponse badRequest: request ]