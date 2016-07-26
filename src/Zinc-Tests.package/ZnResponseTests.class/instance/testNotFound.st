testing
testNotFound
	| response |
	response := ZnResponse notFound: 'http:///secret.txt'.
	self assert: response code = 404.
	self assert: (response headers includesKey: 'Server').
	self assert: (response headers includesKey: 'Date').
	self assert: (response entity contents includesSubstring: 'secret.txt')