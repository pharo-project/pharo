instance-creation
newMergeButton
	"Answer a new button for performing the merge.
	It will be disabled if any conflicts are unresolved."

	^self
		newButtonFor: self
		action:  #merge
		getEnabled:  #allConflictsResolved
		label:  'Merge' translated
		help: 'Merge the version into the image' translated