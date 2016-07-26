lookups
getNoProxyDomains
	"Return a comma seperated string of domains not to proxy"
	"InternetConfiguration getNoProxyDomains"

	^self primitiveGetStringKeyedBy: 'NoProxyDomains'
