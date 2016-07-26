testing
testEchoBinary
	self withServerDo: [ :server | | response entityIn entityOut |
		server reader: [ :stream | ZnRequest readBinaryFrom: stream ].
		entityIn := ZnEntity with: 'ABC' type: 'text/plain'.
		response := ZnEasy put: (server localUrl addPathSegment: 'echo'; yourself)  data: entityIn.
		self assert: (response contentType = ZnMimeType textPlain).
		self assert: (response statusLine code = 200).
		entityOut := ZnEntity with: entityIn string asByteArray type: entityIn contentType.
		self assert: (response entity contents includesSubstring: entityOut printString) ]