private
createPullRequest
	^ IceGitHubPullRequest new 
		credentials: self credentials;
		title: self titlePanel text trim;
		headRemote: self headForkList selectedItem;
		headBranchName: self headBranchList selectedItem;
		baseRemote: self baseForkList selectedItem;
		baseBranchName: self baseBranchList selectedItem;
		body: self bodyPanel text asString trim;
		yourself