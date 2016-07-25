actions
upgrade
	^self class upgradeIsMerge
		ifTrue: [self upgradeByMerging]
		ifFalse: [self upgradeByLoading]