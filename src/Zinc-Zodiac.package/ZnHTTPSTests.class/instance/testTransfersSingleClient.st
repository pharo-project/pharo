testing
testTransfersSingleClient
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	(client := ZnClient new)
		https;
		host: 's3-eu-west-1.amazonaws.com'.
	(Integer primesUpTo: 100) do: [ :each | | size |
		size := 1024 * each + each.
		client
			url: ('/public-stfx-eu/test-', size asString, '.txt');
			get.
		self assert: client isSuccess.
		self assert: (client response contentType matches: ZnMimeType textPlain).
		self assert: client response contentLength equals: size.
		self 
			assert: client contents 
			equals: (self class generateTestData: size)	].
	client close