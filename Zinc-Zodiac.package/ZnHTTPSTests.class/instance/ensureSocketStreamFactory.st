private
ensureSocketStreamFactory
	self 
		assert: ZnNetworkingUtils default secureSocketStreamClass notNil
		description: 'Please set a secure socket stream class in ZnNetworkingUtils default or switch to the ZnZodiacNetworkingUtils socket stream factory'