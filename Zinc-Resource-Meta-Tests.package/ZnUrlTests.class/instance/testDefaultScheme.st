testing
testDefaultScheme
	| url |
	url := ZnUrl fromString: 'www.example.com/foo.html' defaultScheme: #http.  
	self assert: url hasScheme.
	self assert: url scheme = #http.
	self assert: url host = 'www.example.com'.
	self assert: url pathSegments = (OrderedCollection with: 'foo.html')
	