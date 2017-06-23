actions
cloneRepositoryFrom: aRemote branch: aBranchName
	| url |
	
	url := aRemote url.
	LGitGlobal runSequence: [ 
		| repo cloneOptions |
		repo := LGitRepository on: self location.
		cloneOptions := LGitCloneOptions withCredentialsProvider: IceCredentialsProvider default.
		
		repo clone: url options: cloneOptions.
		aBranchName ifNotNil: [ 
			repo checkout: aBranchName ].

		(LGitRemote of: repo named: 'origin')
			lookup;
			setUrl: url ]