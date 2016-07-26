testing
testCookiesParsing
	| input request cookies testCookie1 testCookie2 |
	input := 'GET /foo.html HTTP/1.1', String crlf, 
		'Cookie: testCookie1=123; testCookie2=321', String crlf,
		'Host: foo.com', String crlf, 
		'Agent: SUnit', String crlf, 
		String crlf.
	request := ZnRequest readFrom: input readStream.
	
	cookies := request cookies.
	testCookie1 := cookies detect: [ :each | each name = 'testCookie1' ].
	testCookie2 := cookies detect: [ :each | each name = 'testCookie2' ].
	
	self assert: testCookie1 notNil.
	self assert: testCookie1 value = '123'.	
	
	self assert: testCookie2 notNil.
	self assert: testCookie2 value = '321'.	

