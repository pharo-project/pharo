testing
testMainPage
	self withServerDo: [ :server |
		| client |
		client := ZnClient new.
		client url: server localUrl; addPath: #image.
		client get.
		self assert: client isSuccess.
		self assert: (client entity contentType matches: ZnMimeType textHtml).
		self assert: (client contents includesSubstring: 'Image').
		client close ]