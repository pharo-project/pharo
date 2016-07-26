public
handleRequest: request
	(request method = #GET 
		and: [ 
			request uri firstPathSegment = #sunit 
				and: [ 
					request uri pathSegments size > 1 ] ])
		ifTrue: [ 
			^ self handleGetRequest: request ].
	^ ZnResponse notFound: request uri