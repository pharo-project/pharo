testing
disabledTestPayPal
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	(client := ZnClient new) 
		get: 'https://www.paypal.com'.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'PayPal').
	client close