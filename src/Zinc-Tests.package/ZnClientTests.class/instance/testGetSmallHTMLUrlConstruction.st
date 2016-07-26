testing
testGetSmallHTMLUrlConstruction
	| client |
	(client := ZnClient new)
		http;
		host: self smallHtmlUrl host;
		path: self smallHtmlUrl pathPrintString;
		get.
	self assert: client isSuccess.
	self assert: client response contentType = ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	client close