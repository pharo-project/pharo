testing
testSmall
	self withServerDo: [ :server | 
		| client |
		client := ZnClient new
			url: (server localUrl addPathSegment: #small);
			yourself.
		client get.
		self assert: client isSuccess.
		self assert: client response contentType equals: ZnMimeType textHtml.
		self assert: (client contents includesSubstring: 'Small').
		self assert: (client contents includesSubstring: 'This is a small HTML document').
		client close ]