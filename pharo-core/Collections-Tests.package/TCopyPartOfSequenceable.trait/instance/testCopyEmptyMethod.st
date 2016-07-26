tests - copying part of sequenceable
testCopyEmptyMethod
	| result |
	result := self collectionWithoutEqualElements  copyEmpty .
	self assert: result isEmpty .
	self assert: result class= self nonEmpty class.