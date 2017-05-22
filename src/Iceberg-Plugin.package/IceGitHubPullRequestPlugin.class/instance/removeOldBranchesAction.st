private actions
removeOldBranchesAction
	^ GLMGenericAction new
		action: [ :presentation | self removeBranches: presentation selection repository ];
		category: 'GitHub';
		showTitle: 'Remove old branches...'; 
		yourself
