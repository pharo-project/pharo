testing
testHeadSmallHTML
	| client |
	self withServerDo: [ :server |
		(client := ZnClient new)
			url: server localUrl;
			addPath: 'echo';
			head.
		self assert: client isSuccess.
		self assert: client response contentLength > 0.
		self assert: client response hasEntity not.
		client close ]