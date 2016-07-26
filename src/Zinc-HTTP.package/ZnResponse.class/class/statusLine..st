instance creation
statusLine: statusLine
	^ self new
		statusLine: statusLine;
		headers: ZnHeaders defaultResponseHeaders;
		yourself