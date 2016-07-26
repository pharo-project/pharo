testing
testGetForm
	| client |
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			formAt: 'username' put: 'john';
			formAdd: 'password' -> 'secret';
			get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'john').
		self assert: (client contents includesSubstring: 'secret') ]