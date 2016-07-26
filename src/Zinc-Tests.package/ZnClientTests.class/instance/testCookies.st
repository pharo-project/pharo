testing
testCookies
	| client |
	self withServerDo: [ :server |
		server onRequestRespond: [ :request | | cookie1 cookie2 response |
			cookie1 := request cookies detect: [ :each | each name = 'x' ] ifNone: [ ZnCookie name: 'x' value: '0' ].
			cookie2 := request cookies detect: [ :each | each name = 'y' ] ifNone: [ ZnCookie name: 'y' value: '100' ].
			cookie1 value: (cookie1 value asInteger + 1) asString.
			cookie2 value: (cookie2 value asInteger + 2) asString.
			response := ZnResponse ok: (ZnEntity text: 'OK').
			response addCookie: cookie1; addCookie: cookie2.
			response ].
		client := ZnClient new url: server localUrl; yourself.
		self assert: (client session cookieJar cookieAt: 'x' forUrl: client request url) isNil.
		self assert: (client session cookieJar cookieAt: 'y' forUrl: client request url) isNil.
		client get.
		self assert: client isSuccess.
		self assert: (client session cookieJar cookieAt: 'x' forUrl: client request url) value = '1'.
		self assert: (client session cookieJar cookieAt: 'y' forUrl: client request url) value = '102'.
		client get.
		self assert: client isSuccess.
		self assert: (client session cookieJar cookieAt: 'x' forUrl: client request url) value = '2'.
		self assert: (client session cookieJar cookieAt: 'y' forUrl: client request url) value = '104'.
		client close ]