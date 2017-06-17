merging
run
	"Merge selected commit into another branch"
	self repository isModified ifTrue: [ 
		self confirmContinueWithDirtyRepository ifFalse: [ ^ self ]].

	self chooseTargetBranch ifNil: [ ^ self ].

	[ source mergeInto: self targetBranch ]
		on: IceMergeAborted
		do: [ 
			self targetBranch name = repository branchName 
				ifTrue: [ repository mergeConflictsWith: source ]
				ifFalse: [ self informAutomaticMergeImpossible. ^ self ]].

	self onMerge value 
