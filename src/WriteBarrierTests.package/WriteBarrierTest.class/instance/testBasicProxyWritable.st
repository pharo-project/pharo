tests - proxy
testBasicProxyWritable
	self alwaysWritableObjects , self maybeReadOnlyObjects do: [ :each |
		self assert: (self mirror isReadOnlyObject: each) equals: false ]