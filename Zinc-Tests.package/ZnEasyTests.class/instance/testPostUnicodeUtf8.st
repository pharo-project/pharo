testing
testPostUnicodeUtf8
	self withServerDo: [ :server | | response data |
		data := String streamContents: [ :stream | 0 to: 16r024F do: [ :each | stream nextPut: each asCharacter ] ].
		response := ZnEasy post: (server localUrl addPathSegment: 'echo'; yourself) data: (ZnEntity text: data).
		self assert: (response headers contentType = ZnMimeType textPlain).
		self assert: (response statusLine code = 200).
		self assert: (response entity string includesSubstring: 'POST').
		self assert: (response entity string includesSubstring: data) ]