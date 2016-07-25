instance creation
method: method url: urlObject
	| url |
	url := urlObject asZnUrl.
	^ self new
		requestLine: (ZnRequestLine method: method uri: url);
		headers: (ZnHeaders requestHeadersFor: url);
		yourself