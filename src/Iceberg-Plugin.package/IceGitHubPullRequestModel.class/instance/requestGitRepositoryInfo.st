private request
requestGitRepositoryInfo
	| remote |
	remote := self headRemote.
	^ IceGitHubAPI new 
		credentials: self credentials;
		requestRepository: remote owner 
			project: remote projectBasename
