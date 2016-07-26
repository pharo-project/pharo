methods
contains
	^((1 to: 10) detect: [:each | each > 2] ifNone: [nil]) isNil