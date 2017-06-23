actions
removeBranch: json
	| branchName |
	
	branchName := json at: 'name'.
	IceGitHubAPI new 
		credentials: self credentials;
		deleteBranch: self remote owner 
			project: self remote projectBasename
			name: branchName.
	UIManager default inform: ('Branch {1} removed.' format: { branchName })		
	