testing
isRequestAuthenticated: request
	| authorization response cresponse a1 a2 nonce opaque |
	authorization := self class parseAuthRequest: (request headers at: 'Authorization' ifAbsent: [ ^ false ]).
	nonce := authorization at: 'nonce' ifAbsent: [ ^ false ].
	opaque := self nonces at: nonce ifAbsent: [ ^ false ].
	a1 := self a1for: (authorization at: 'username' ifAbsent: [ ^ false ]).
	a1 ifNil: [ ^ false ].
	a2 := self a2forUrl: (authorization at: 'uri' ifAbsent: [ '/' ]) method: request method.
	response := self class md5Hash: (a1, ':', nonce, ':', a2).
	"(opaque = (authorization at: 'opaque' ifAbsent: [^false])) ifFalse: [^false]."
	cresponse := authorization at: 'response' ifAbsent: [ ^ false ].
	^ (response = cresponse)