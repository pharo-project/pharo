public
handleRequest: request
	"Dispatch between GET and POST on /image"
	
	request uri path = #image
		ifTrue: [ 
			request method = #GET
				ifTrue: [ 
					^ self handleGetRequest: request ].
			request method = #POST
				ifTrue: [ 
					^ self handlePostRequest: request ] ].
	^ ZnResponse notFound: request uri