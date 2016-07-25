testing
testDelete
	self withServerDo: [ :server | | response |
		response := ZnEasy delete: (server localUrl addPathSegments: #('echo' 'foo'); yourself).
		self assert: (response headers contentType = ZnMimeType textPlain).
		self assert: (response statusLine code = 200).
		self assert: (response entity string includesSubstring: 'DELETE').
		self assert: (response entity string includesSubstring: 'foo') ]