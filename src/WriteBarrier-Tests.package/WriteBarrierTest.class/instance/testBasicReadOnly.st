tests - object
testBasicReadOnly
	self alwaysReadOnlyObjects do: [ :each |
		self assert: each isReadOnlyObject equals: true ]