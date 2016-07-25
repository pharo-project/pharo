private protocol
executeWithRedirectsRemaining: redirectCount
	self getConnectionAndExecute.
	response isRedirect 
		ifTrue: [
			(redirectCount > 0 and: [ self followRedirects ])
				ifTrue: [
					self 
						prepareRedirect; 
						executeWithRedirectsRemaining: redirectCount - 1 ]
				ifFalse: [
					self followRedirects 
						ifTrue: [ ZnTooManyRedirects signal ] ] ].
	^ self handleResponse
	