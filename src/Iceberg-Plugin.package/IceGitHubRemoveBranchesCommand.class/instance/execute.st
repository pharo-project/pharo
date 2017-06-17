execute
execute 
	UIManager default 
		informUser: ('Retrieving branch information from {1}' format: { self remote url })
		during: [ self cacheAllBranches ].
	IceGitHubSelectListModel new 
		setModal: true;
		title: ('Select branches to remove on remote {1}' format: {self remote remoteName});
		selectLabel: 'Remove branches';
		items: self remoteBranches;
		displayBlock: [ :each | 
			'{1} ({2})' format: { 
				each at: 'name'. 
				self timeSinceLastCommit: each } ];
		onAccept: [ :selection | self removeBranches: selection ];
		openWithSpec