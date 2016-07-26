testing
isRequestAuthenticated: request
	| authorization username password |
	authorization := [ request basicAuthentication ] on: Error do: [ ^ false ].
	username := authorization first.
	password := authorization second.
	^ (credentials at: username ifAbsent: [ nil ]) = password
	