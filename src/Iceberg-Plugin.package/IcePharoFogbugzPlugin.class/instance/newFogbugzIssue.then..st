actions
newFogbugzIssue: aRepository then: aBlock
	IcePharoFogbugzModel new 
		onAccept: [ :branchName | 
			aRepository createBranch: branchName.
			aBlock value ];
		openWithSpec.
