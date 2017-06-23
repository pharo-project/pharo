private actions
newPullRequestAction
	^ GLMGenericAction new
		action: [ :presenter | self newPullRequest: presenter selection repository ];
		category: 'GitHub';
		showTitle: 'Create pull request...'; 
		yourself
