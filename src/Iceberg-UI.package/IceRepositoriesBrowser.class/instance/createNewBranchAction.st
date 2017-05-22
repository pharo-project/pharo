menu actions - selected repository
createNewBranchAction
	^ GLMGenericAction new
		action: [ :presentation | 
			presentation selection createNewBranchThen: [] ];
		selectionCondition: #isOperative;
		showTitle: 'Create new branch...';
		shortcut: $B.
