testing
testGetSmallHTML
	| client |
	(client := ZnClient new)
		url: self smallHtmlUrl;
		get.
	self assert: client isSuccess.
	self assert: client isContentTypeAcceptable.
	self assert: client response contentType = ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	self assert: client isConnected.
	client close.
	self deny: client isConnected