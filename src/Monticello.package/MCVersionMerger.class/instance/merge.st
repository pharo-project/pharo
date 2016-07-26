actions
merge

	self gatherChanges.

	self resolveConflicts ifTrue:
		[merger load.
		records do: [:ea | ea updateWorkingCopy].
		^ true].
	
	^ false