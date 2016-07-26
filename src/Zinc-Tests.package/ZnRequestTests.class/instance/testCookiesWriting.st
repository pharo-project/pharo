testing
testCookiesWriting
	|  client testCookie1 testCookie2 cookieString cookies|
	client := ZnClient new.
	client url: 'http://www.example.com'.
	(testCookie1 := ZnCookie name: 'testCookie1' value: '123')
		domain: 'example.com'.
	(testCookie2 := ZnCookie name: 'testCookie2' value: '321')
		domain: 'example.com'.
	(client session cookieJar)
		add: testCookie1;
		add: testCookie2.
	client sendCookies.
	cookies := client request cookies do: [ :each | each defaultDomain: 'example.com' ].
	self assert: (cookies includes: testCookie1).
	self assert: (cookies includes: testCookie2)
	
		
