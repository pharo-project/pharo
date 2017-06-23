menu actions - branches
branchCreateNewAction
	^ GLMGenericAction new
		action: [ :presentation :model | 
			model createNewBranchThen: [ presentation pane browser update ] ];
		icon: #add asIcon;
		showTitle: 'Create branch'; 
		yourself