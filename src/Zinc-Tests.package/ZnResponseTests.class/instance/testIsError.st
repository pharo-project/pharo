testing
testIsError
	| response |
	response := ZnResponse ok: (ZnStringEntity html: '<h1>hello world</h1>').
	self assert: response isError not.
	response := ZnResponse notFound: 'http:///secret.txt'.
	self assert: response isError.
	response := ZnResponse redirect: '/something/else/foo.txt'.
	self assert: response isError not