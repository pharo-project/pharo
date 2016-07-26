testing
testTransfers
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	(Integer primesUpTo: 100) do: [ :each | | size client |
		size := 1024 * each + each.
		(client := ZnClient new)
			https;
			host: 's3-eu-west-1.amazonaws.com';
			addPath: 'public-stfx-eu';
			addPath: ('test-', size asString, '.txt');
			get;
			close.
		self assert: client isSuccess.
		self assert: (client response contentType matches: ZnMimeType textPlain).
		self assert: client response contentLength equals: size.
		self 
			assert: client contents 
			equals: (self class generateTestData: size)	]