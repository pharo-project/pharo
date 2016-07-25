tests - empty
testIfEmptyifNotEmpty

	self assert: (self empty ifEmpty: [true] ifNotEmpty: [false]).
	self assert: (self nonEmpty ifEmpty: [false] ifNotEmpty: [true]).
	