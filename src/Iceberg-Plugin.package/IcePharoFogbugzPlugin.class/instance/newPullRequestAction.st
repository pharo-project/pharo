private actions
newPullRequestAction
	"This actions are copied from GitHub plugin, but presented here to make it easier"
	^ GLMGenericAction new
		action: [ :presenter | self newPullRequest: presenter selection repository ];
		category: 'Pharo';
		showTitle: 'Create pull request...'; 
		yourself
