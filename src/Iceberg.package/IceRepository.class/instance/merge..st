actions
merge: aCommitish
	"Currently a commitish can only be an IceCommitInfo, but we should be prepared for other types of commitish."

	[ self basicMerge: aCommitish ]
		on: IceMergeAborted
		do: [ :error | self mergeConflictsWith: aCommitish ] 

