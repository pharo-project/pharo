tests
testIsNativeSSLPluginPresent
	"This test succeeds when the SSL VM plugin can be instantiated and intialized."
	
	self 
		assert: self isNativeSSLPluginPresent 
		description: 'It seems that the native VM SSL plugin cannot be instantiated and initialized. (VM plugin missing ? OS libraries missing ?)'