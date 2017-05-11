tests
testNonCodeChangeEvent

	monitor logEntryCommented:
		(EpLogEntriesComment 
			entryReferences: #()
			oldComment: ''
			newComment: '42').

	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.
