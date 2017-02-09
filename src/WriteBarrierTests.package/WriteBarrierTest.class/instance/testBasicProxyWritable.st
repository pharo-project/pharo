tests - proxy
testBasicProxyWritable
	self alwaysWritableObjects , self maybeReadOnlyObjects do: [ :each |
		self assert: (MirrorPrimitives isObjectReadOnly: each) equals: false ]