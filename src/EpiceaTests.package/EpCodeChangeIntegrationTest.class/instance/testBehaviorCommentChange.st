tests
testBehaviorCommentChange

	aClass := classFactory newClass.

	self assert: (self countLogEventsWith: EpBehaviorCommentChange) equals: 0.
	
	aClass classComment: 'hey!' stamp: DateAndTime now.

	self assert: (self countLogEventsWith: EpBehaviorCommentChange) equals: 1.