tests
testBehaviorCommentChange

	| aClass |
	aClass := classFactory newClass.
	aClass classComment: 'before'.
	aClass classComment: 'after'.
	self setHeadAsInputEntry.

	self revertInputEntry.
	
	self assert: aClass organization classComment equals: 'before'.
