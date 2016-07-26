tests - copying part of sequenceable
testCopyAfterLastEmpty
	| result |
	result := self empty copyAfterLast: self collectionWithoutEqualElements first.
	self assert: result isEmpty.