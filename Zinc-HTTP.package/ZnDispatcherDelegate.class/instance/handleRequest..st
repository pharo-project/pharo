public
handleRequest: request
	| handler |
	handler := self dispatcherDictionary 
		at: request uri pathPrintString 
		ifAbsent: [ ^ ZnResponse notFound: request uri ].	
	^ handler 
		value: request 
		value: (ZnResponse new
			statusLine: (ZnStatusLine ok);
			headers: ZnHeaders defaultResponseHeaders;
			yourself).

