testing
testWikimedia
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ]. 
	(client := ZnClient new) 
		get: 'https://secure.wikimedia.org/'.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'Wikimedia').
	client close