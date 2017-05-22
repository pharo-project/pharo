building
addRepositoryBranchesTo: composite
	| branches |
	
	(branches := composite fastTable)
		title: 'Branches';
		display: #branches;
		column: 'Name' evaluated: [ :branch :model | model descriptionTextForBranch: branch ];
		column: 'Status' evaluated: [ :branch :model | model statusForBranch: branch ].
		
	"actions"
	branches addAction: self branchCreateNewAction.
	"selection actions"
	branches 
		addSelectionAction: self branchSwitchAction;
		addSelectionAction: self branchCompareWithCurrentAction	