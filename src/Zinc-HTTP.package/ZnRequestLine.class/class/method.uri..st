instance creation
method: method uri: uri
	^ self new
		method: method;
		uri: uri;
		version: ZnConstants defaultHTTPVersion;
		yourself 