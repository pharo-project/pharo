testing
testOSAssignedPort
	| server | 
	server := ZnServer on: 0.
	self assert: server port equals: 0.
	[ 
		| response |
		server start.
		self assert: server port > 0. 
		response := ZnEasy get: server localUrl / #echo / #foo.
		self assert: (response contentType = ZnMimeType textPlain).
		self assert: (response statusLine code = 200).
		self assert: (response entity contents includesSubstring: 'Zinc').
		self assert: (response entity contents includesSubstring: 'foo').
		self assert: (response entity contents includesSubstring: server printString) ] 
			ensure: [ server stop ]