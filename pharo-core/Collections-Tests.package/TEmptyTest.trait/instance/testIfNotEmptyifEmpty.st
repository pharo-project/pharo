tests - empty
testIfNotEmptyifEmpty

	self assert: (self empty ifNotEmpty: [false] ifEmpty: [true]).
	self assert: (self nonEmpty ifNotEmpty: [true] ifEmpty: [false]).
	