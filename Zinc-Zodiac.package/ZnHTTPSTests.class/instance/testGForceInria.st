testing
testGForceInria
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	(client := ZnClient new)
		get: 'http://gforge.inria.fr/frs/?group_id=1299'.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'inria').
	client close