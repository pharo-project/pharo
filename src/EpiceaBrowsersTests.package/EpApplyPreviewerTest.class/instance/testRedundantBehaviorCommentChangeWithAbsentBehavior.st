tests
testRedundantBehaviorCommentChangeWithAbsentBehavior

	| aClass |
	aClass := classFactory newClass.
	aClass classComment: 'before'.
	self setHeadAsInputEntry.
	aClass removeFromSystem.

	self assertEmptyPreviewLog.