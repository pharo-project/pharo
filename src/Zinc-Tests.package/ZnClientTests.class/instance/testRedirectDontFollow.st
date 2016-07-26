testing
testRedirectDontFollow
	| client target |
	target := 'http://zn.stfx.eu'.
	(client := ZnClient new) 
		dontFollowRedirects;
		get: target.
	self assert: client response isRedirect.
	client
		enforceHttpSuccess: true;
		get: target.
	self assert: client response isRedirect.
	client close