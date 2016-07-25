settings
isHttpProxyException: serverName
	"Return true when serverName matches any of our HTTP proxy exceptions, else return false"

	^ self httpProxyExceptions anySatisfy: [ :domainName | domainName match: serverName ]