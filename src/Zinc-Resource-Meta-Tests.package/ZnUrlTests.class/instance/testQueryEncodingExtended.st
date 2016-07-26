testing
testQueryEncodingExtended
	| url |
	url := 'http://server.com/foo' asZnUrl.
	url 
		queryAt: 'one' put: '1';
		queryAt: 'equalSign' put: 'a=0';
		queryAt: 'ampersand' put: 'm&m';
		queryAt: 'questionMark' put: 'x?-1'.
	url := url asString asZnUrl.
	self assert: (url queryAt: 'one') equals: '1'.
	self assert: (url queryAt: 'equalSign') equals: 'a=0'.
	self assert: (url queryAt: 'ampersand') equals: 'm&m'.
	self assert: (url queryAt: 'questionMark') equals: 'x?-1'
