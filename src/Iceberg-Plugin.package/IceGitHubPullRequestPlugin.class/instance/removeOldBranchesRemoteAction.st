private actions
removeOldBranchesRemoteAction
	^ GLMGenericAction new
		action: [ :presentation :model | 
			self removeBranches: model remote: presentation selection ];
		category: 'GitHub';
		showTitle: 'Remove old branches...'; 
		yourself
