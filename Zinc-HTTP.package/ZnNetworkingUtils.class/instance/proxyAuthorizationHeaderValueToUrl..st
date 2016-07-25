proxy
proxyAuthorizationHeaderValueToUrl: url
	"Answer the Proxy-Authorization header value for HTTP access to url, or nil if none"
	
	^ (self isProxyAuthorizationRequired and: [ self shouldProxyUrl: url ])
		ifTrue: [ 'Basic ', (ZnUtils encodeBase64: (self httpProxyUser, ':', self httpProxyPassword)) ] 
		ifFalse: [ nil ]  