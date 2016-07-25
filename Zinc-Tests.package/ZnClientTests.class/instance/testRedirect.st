testing
testRedirect
	| client response target |
	target := 'http://zn.stfx.eu'.
	client := ZnClient new url: target.
	client get.
	self assert: client isSuccess.
	client
		close;
		maxNumberOfRedirects: 0;
		url: target.
	self should: [ client get ] raise: ZnTooManyRedirects.
	client close.
	response := [ 
		ZnClient new
			beOneShot;
			maxNumberOfRedirects: 0;
			get: target;
			response ] on: ZnTooManyRedirects do: [ :exception | exception resume ].
	self assert: response isRedirect