tests - copying part of sequenceable
testCopyUpToEmpty
	| result |
	result := self empty copyUpTo: self collectionWithoutEqualElements first.
	self assert: result isEmpty.
	