settings
proxyPassword
	"Return the HTTP proxy password to use, if any. Can be empty."

	^ ProxyPassword 
		ifNil: [ '' ]
		ifNotNil: [ ProxyPassword base64Decoded ]