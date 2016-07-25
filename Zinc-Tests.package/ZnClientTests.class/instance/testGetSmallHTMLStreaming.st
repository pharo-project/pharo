testing
testGetSmallHTMLStreaming
	| client result contents |
	result := (client := ZnClient new)
		url: self smallHtmlUrl;
		streaming: true;
		get.
	self assert: client isSuccess.
	self assert: client response contentType = ZnMimeType textHtml.
	self assert: result isStream.
	self assert: client entity stream = result.
	contents := ZnUTF8Encoder new decodeBytes: result upToEnd.
	self assert: (contents includesSubstring: 'Small').
	client close