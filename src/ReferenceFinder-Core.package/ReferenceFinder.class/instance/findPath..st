accessing
findPath: aBlock 
	foundBlock := [:path | ^path].
	testBlock := aBlock.
	self find.
	^nil