settings
proxyUser
	"Return the HTTP proxy user to use, if any. Can be empty."

	^ ProxyUser
		ifNil: [ '' ]
		ifNotNil: [ ProxyUser base64Decoded ]