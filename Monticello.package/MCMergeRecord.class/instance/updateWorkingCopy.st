actions
updateWorkingCopy
	self isAncestorMerge ifFalse:
		[self imageIsClean
			ifTrue: [version workingCopy loaded: version]
			ifFalse: [version workingCopy merged: version]]