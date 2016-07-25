public
authHeader
	| nonce opaque |
	nonce := self createNonce.
	opaque := self createOpaque.
	self nonces at: nonce put: opaque.
	^ 'Digest realm="', self realm, '", nonce="', nonce, '", opaque="', opaque, '"'