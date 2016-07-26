tests
testPackageIsString

	self definitions do: [ :each |
		self assert: each package isString.
		self assert: each package isNotEmpty ]