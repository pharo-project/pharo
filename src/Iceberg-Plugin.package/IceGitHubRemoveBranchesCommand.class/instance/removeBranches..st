actions
removeBranches: aCollection
	[ 
		aCollection do: [ :each | self removeBranch: each ] ]
	on: IceGitHubError do: [ :e | self reportError: e ]