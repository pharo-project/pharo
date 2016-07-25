testing
testGetSmallHTMLDocument
	| url response |
	url := self smallHtmlUrl.
	response := ZnEasy get: url.
	self assert: (response headers contentType = ZnMimeType textHtml).
	self assert: (response statusLine code = 200).
	self assert: (response entity string includesSubstring: 'small')