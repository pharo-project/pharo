request handling
notFoundResponse: message
	^ ZnResponse new
		statusLine: ZnStatusLine notFound;
		headers: ZnHeaders defaultResponseHeaders;
		entity: (ZnEntity textCRLF: 'Not Found: ', message);
		yourself.