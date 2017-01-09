releasing
releaseLog: aLogCreatedByThisBuilder
	| logFileReference |
	logFileReference := aLogCreatedByThisBuilder store fileReference.
	logFileReference ensureDelete.
	logFileReference parent hasChildren ifFalse: [
		logFileReference parent ensureDelete ].