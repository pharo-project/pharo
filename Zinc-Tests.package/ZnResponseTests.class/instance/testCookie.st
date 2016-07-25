testing
testCookie
	| response cookie1  responseString |
	response := ZnResponse ok: (ZnStringEntity html: '<h1>hello world</h1>').
	cookie1 := ZnCookie new.
	cookie1 
		name: 'testCookie1';
		value: '123'.
		
	response addCookie: cookie1.
	
	responseString := String streamContents: [ :stream | response writeOn: stream ].
	
	self assert: (responseString includesSubstring: 'Set-Cookie: testCookie1=123')	

	
		
		
