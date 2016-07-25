private protocol
handleResponse
	self receiveCookies.
	(self enforceHttpSuccess and: [ response isSuccess not ])
		ifTrue: [ 
			(self followRedirects not and: [ response isRedirect ])
				ifFalse: [ (ZnHttpUnsuccessful response: response) signal ] ].
	(self enforceAcceptContentType and: [ self isContentTypeAcceptable not ])
		ifTrue: [ 
			(ZnUnexpectedContentType 
				expected: request accept asZnMimeType 
				actual: response contentType) signal ].
	^ self contents