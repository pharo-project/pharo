actions
mergeWithNameLike: baseName

	self gatherChanges.
	
	self resolveConflicts ifTrue:
		[merger loadWithNameLike: baseName.
		records do: [:ea | ea updateWorkingCopy]].