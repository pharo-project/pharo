tests - proxy
testBasicProxyReadOnly
	self alwaysReadOnlyObjects do: [ :each |
		self assert: (self mirror isReadOnlyObject: each) equals: true ]