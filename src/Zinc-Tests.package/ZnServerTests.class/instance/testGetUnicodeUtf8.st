testing
testGetUnicodeUtf8
	self withServerDo: [ :server | | response html |
		response := ZnEasy get: (server localUrl addPathSegment: 'unicode'; yourself).
		self assert: (response contentType = ZnMimeType textHtml).
		self assert: (response statusLine code = 200).
		self assert: response contentType isCharSetUTF8.
		html := response entity contents.
		self assert: (html includesSubstring: 'Unicode').
		0 to: 16r024F do: [ :each | self assert: (html includes: each asCharacter) ] ]