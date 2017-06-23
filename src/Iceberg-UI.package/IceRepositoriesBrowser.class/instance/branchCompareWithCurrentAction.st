menu actions - branches
branchCompareWithCurrentAction
	^ GLMGenericAction new
		action: [ :presentation :model | 
			model compareCurrentBranchWithBranch: presentation selection. 
			presentation pane browser update ];
		showTitle: 'Compare with current branch'; 
		yourself
