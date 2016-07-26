testing
testHeadSmallHTMLDocument
	| url response |
	url := self smallHtmlUrl.
	response := ZnEasy head: url.
	self assert: (response headers contentType = ZnMimeType textHtml).
	self assert: (response statusLine code = 200).
	self assert: response hasEntity not