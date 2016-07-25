testing
testAmazonAWS
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	(client := ZnClient new) 
		get: 'https://aws.amazon.com/'.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'Amazon').
	client close