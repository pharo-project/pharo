testing
testPut
	self withServerDo: [ :server | | response data |
		data := String streamContents: [ :stream | 1 to: 32 do: [ :each | stream nextPut: 'abc' atRandom ] ].
		response := ZnEasy put: (server localUrl addPathSegment: 'echo'; yourself) data: (ZnEntity text: data).
		self assert: (response headers contentType = ZnMimeType textPlain).
		self assert: (response statusLine code = 200).
		self assert: (response entity string includesSubstring: 'PUT').
		self assert: (response entity string includesSubstring: data) ]