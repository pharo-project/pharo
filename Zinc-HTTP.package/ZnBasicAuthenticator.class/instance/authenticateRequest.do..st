public
authenticateRequest: request do: block
	^ (self isRequestAuthenticated: request)
		ifTrue: [ block value ]
		ifFalse: [ ZnResponse unauthorized: self authHeader ]