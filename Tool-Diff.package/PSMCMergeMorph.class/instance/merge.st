actions
merge
	"Do the merge and close if no conflicts."
	
	^self model isMerged
		ifTrue: [self merged: true.
				self window delete]
		ifFalse: [self inform: 'You must resolve all conflicts first.' translated]