request handling
authenticateAndDelegateRequest: request
	"Handle request and return a response.
	If we have a delegate, pass the responsability.
	If we have no delegate, we return a page not found.
	Make sure to pass via our authenticator."

	^ self
		authenticateRequest: request
		do: [ 
			self delegate
				ifNil: [ ZnResponse notFound: request uri ]
				ifNotNil: [ :delegate | delegate handleRequest: request ] ]