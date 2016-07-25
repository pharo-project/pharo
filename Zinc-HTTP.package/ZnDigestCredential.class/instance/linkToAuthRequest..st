operations
linkToAuthRequest: aHeader
	| data |
	data := self parseAuthRequest: aHeader.
	self nonce: (data at: 'nonce').
	self realm: (data at: 'realm' ifAbsent: [ '' ])