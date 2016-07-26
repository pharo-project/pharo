testing
testPostTwice
	| client |
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			contents: 'Foo bar!';
			post.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Foo bar!').
		client
			contents: 'Another one ?';
			post.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'Another one ?').
		client close ]