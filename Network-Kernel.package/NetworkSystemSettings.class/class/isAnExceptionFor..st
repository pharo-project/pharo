settings
isAnExceptionFor: url
	"Return true when the host of url matches any of our HTTP proxy exceptions, else return false"

	 ^ self isHttpProxyException: url host