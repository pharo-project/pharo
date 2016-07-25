testing
testGetSmallHTMLOneShot
	| client |
	(client := ZnClient new)
		beOneShot;
		url: self smallHtmlUrl;
		get.
	self assert: client isSuccess.
	self assert: client response contentType = ZnMimeType textHtml.
	self assert: (client contents includesSubstring: 'Small').
	self deny: client isConnected