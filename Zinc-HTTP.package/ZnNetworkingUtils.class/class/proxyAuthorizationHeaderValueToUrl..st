proxy
proxyAuthorizationHeaderValueToUrl: url
	"Answer the Proxy-Authorization header value for HTTP access to url, or nil if none"
	
	^ self default proxyAuthorizationHeaderValueToUrl: url  