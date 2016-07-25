lookups
getHTTPProxyHost
	"Return the http proxy for this client."
	"InternetConfiguration getHTTPProxyHost"

	^self primitiveGetStringKeyedBy: 'HTTPProxyHost'
