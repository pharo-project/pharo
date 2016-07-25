tests - empty
testIfNotEmpty

	self empty ifNotEmpty: [self assert: false].
	self nonEmpty ifNotEmpty: [self assert: true].
	self assert: (self nonEmpty ifNotEmpty: [:s | s ]) = self nonEmpty
	