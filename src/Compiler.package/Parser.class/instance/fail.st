error handling
fail

	| exitBlock |
	(encoder isNil or: [encoder == self])
		ifFalse: [encoder release. encoder := nil]. "break cycle"
	exitBlock := failBlock.
	failBlock := nil.
	^exitBlock value