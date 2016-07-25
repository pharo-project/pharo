HTTP Proxy
setHttpProxy

	(self getHTTPProxyHost findTokens: ':') ifNotEmpty: [ :p |
		NetworkSystemSettings
			httpProxyServer: p first;
			httpProxyPort: p second asInteger;
			useHTTPProxy: true ]