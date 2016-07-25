settings
manageHttpProxyExceptions: aStringWithExceptions
	self removeAllHttpProxyExceptions.
	aStringWithExceptions	ifNotNil:[
		LastHTTPProxyExceptions := aStringWithExceptions.
		( ';' split: aStringWithExceptions) do: [
			:domain  | 
			self addHttpProxyException: domain
		].
	].