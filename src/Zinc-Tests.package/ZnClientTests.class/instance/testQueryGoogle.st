testing
testQueryGoogle
	| client |
	(client := ZnClient new)
		http;
		host: 'www.google.com';
		addPath: 'search';
		queryAt: 'q' put: 'Pharo Smalltalk';
		get.
	self assert: client isSuccess.
	self assert: (client response contentType matches: ZnMimeType textHtml).
	self assert: (client contents includesSubstring: 'pharo.org').
	client close