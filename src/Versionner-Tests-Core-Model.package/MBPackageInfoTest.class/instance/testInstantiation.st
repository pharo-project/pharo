tests
testInstantiation
	self should: [ MBPackageInfo new ] raise: Error.

	self should: [ MBPackageInfo named: #'FooBar' ] raise: AssertionFailure.
	self shouldnt: [ MBPackageInfo named: 'FooBar' ] raise: Error.