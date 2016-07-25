testing
testConstruction
	| client |
	(client := ZnClient new)
		http;
		host: 'www.example.com';
		port: 8080;
		addPath: 'foo';
		addPath: 'a space';
		addPath: 'file.txt';
		queryAt: 'key' put: '123456';
		headerAt: 'X-token' put: 'ABCDEF'.
	self assert: client request url host = 'www.example.com'.
	self assert: client request url port = 8080.
	self assert: client request url pathQueryFragmentPrintString = '/foo/a%20space/file.txt?key=123456'.
	self assert: (client request headers at: 'X-token') = 'ABCDEF'