testing
testPrintingSimple
	| url |
	(url := ZnUrl new)
		scheme: #http;
		host: 'www.seaside.st';
		port: 8080;
		addPathSegment: 'example';
		addPathSegment: 'foo.html';
		queryAt: 'q' put: '100';
		fragment: 'mark'.
	self assert: url authority = 'www.seaside.st:8080'.
	self assert: url isAbsolute.
	self assert: url isFilePath.
	self assert: url printString = 'http://www.seaside.st:8080/example/foo.html?q=100#mark'.
	self assert: url file = 'foo.html'.
	self assert: url directory = 'example'.
	self assert: url pathPrintString = '/example/foo.html'.
	self assert: url pathQueryFragmentPrintString = '/example/foo.html?q=100#mark'