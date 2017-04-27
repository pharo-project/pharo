tests
testBehaviorCommentChange

	| aClass |
	aClass := classFactory newClass.
	aClass classComment: 'before'.
	self setHeadAsInputEntry.
	aClass classComment: 'after'.

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpBehaviorCommentChange.
		self assert: output oldComment equals: 'after'.
		self assert: output newComment equals: 'before'.
		]
