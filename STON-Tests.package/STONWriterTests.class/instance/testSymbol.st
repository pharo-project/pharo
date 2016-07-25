tests
testSymbol
	self assert: (self serialize: #foo) = '#foo'.
	self assert: (self serialize: #FOO) = '#FOO'.
	self assert: (self serialize: #bytes) = '#bytes'.
	self assert: (self serialize: #'foo.bar') = '#foo.bar'.
	self assert: (self serialize: #'foo-bar') = '#foo-bar'.
	self assert: (self serialize: #'foo_bar') = '#foo_bar'.
	self assert: (self serialize: #'foo/bar') = '#foo/bar'.
	self assert: (self serialize: #'foo bar') = '#''foo bar'''.
	self assert: (self serialize: #foo123) = '#foo123'.