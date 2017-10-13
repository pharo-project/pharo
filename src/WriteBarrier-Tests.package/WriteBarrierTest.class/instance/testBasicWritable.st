tests - object
testBasicWritable
	self alwaysWritableObjects , self maybeReadOnlyObjects do: [ :each |
		self assert: each isReadOnlyObject equals: false ]