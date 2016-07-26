testing
testCookies
	| response cookie1 cookie2 responseString |
	response := ZnResponse ok: (ZnStringEntity html: '<h1>hello world</h1>').
	cookie1 := ZnCookie new.
	cookie1 
		name: 'testCookie1';
		value: '123'.
		
	cookie2 := ZnCookie new.
	cookie2 
		name: 'testCookie2';
		value: '321'.
		
	response addCookie: cookie1.
	response addCookie: cookie2.
	
	responseString := String streamContents: [ :stream | response writeOn: stream ].
	
	self assert: (responseString includesSubstring: 'Set-Cookie: testCookie1=123').  
	self assert: (responseString includesSubstring: 'Set-Cookie: testCookie2=321').  	

	
		
		
