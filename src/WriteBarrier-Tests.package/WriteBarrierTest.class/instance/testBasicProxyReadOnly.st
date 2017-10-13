tests - proxy
testBasicProxyReadOnly
	self alwaysReadOnlyObjects do: [ :each |
		self assert: (MirrorPrimitives isObjectReadOnly: each) equals: true ]