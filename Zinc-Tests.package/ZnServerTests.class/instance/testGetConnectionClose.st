testing
testGetConnectionClose
	self withServerDo: [ :server | | client |
		client := ZnClient new.
		client url: server localUrl; addPathSegment: #echo.
		client request setConnectionClose.
		client get.
		self assert: client isSuccess.
		self assert: (client contents includesSubstring: 'echo').
		self assert: client response isConnectionClose.
		client close ]