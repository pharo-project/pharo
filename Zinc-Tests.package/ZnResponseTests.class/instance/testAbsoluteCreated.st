testing
testAbsoluteCreated
	| response uri |
	uri := 'http://www.example.com:8888/something/else/foo.txt'.
	response := ZnResponse created: uri asZnUrl.
	self assert: response code = 201.
	self assert: response hasHeaders.
	self assert: (response headers at: 'Location') = uri.
	self assert: response hasEntity.
	self assert: (response entity contents includesSubstring: uri)