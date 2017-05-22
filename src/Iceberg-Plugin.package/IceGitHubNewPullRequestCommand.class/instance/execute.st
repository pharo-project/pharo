execute
execute 
	(self validateMakePullRequestOn: self repository) ifFalse: [ ^ #() ].
	
	(IceGitHubPullRequestModel 
		repository: self repository 
		credentials: self credentials
		headRemote: self remote)
		setModal: true; 
		onAccept: [ :pullRequest | 
			[ 
				pullRequest send.
				UIManager default inform: 'Pull request created.' ]
			on: IceGitHubError do: [ :e | self reportError: e ] ];
		openWithSpec 