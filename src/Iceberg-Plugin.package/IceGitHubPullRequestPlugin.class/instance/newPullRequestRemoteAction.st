private actions
newPullRequestRemoteAction
	^ GLMGenericAction new
		action: [ :presentation :model |
			self newPullRequest: model repository remote: presentation selection ];
		category: 'GitHub';
		showTitle: 'Create pull request...'; 
		yourself
