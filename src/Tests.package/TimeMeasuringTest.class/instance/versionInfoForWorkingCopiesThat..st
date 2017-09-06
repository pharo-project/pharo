utilities
versionInfoForWorkingCopiesThat: wcPredicate 
	^(MCWorkingCopy allManagers select: wcPredicate) inject: ''
		into: [:s :e | s , e description]