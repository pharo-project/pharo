testing
testGetSmallHTMLTwice
	| client |
	(client := ZnClient new)
		url: self smallHtmlUrl;
		get.
	self assert: client isSuccess.
	self assert: client response contentType = ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	client 
		url: self smallHtmlUrl pathPrintString;
		get.
	self assert: client isSuccess.
	self assert: client response contentType = ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	client close