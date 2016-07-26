settings
httpProxyExceptions
	"Return a list of partial, wildcard domain name strings that define which servers should not be proxied"

	^ HTTPProxyExceptions ifNil: [ HTTPProxyExceptions := OrderedCollection new ]