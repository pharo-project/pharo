actions
fetchFrom: aRemote
	self withRepoDo: [ :repo | 
		(LGitRemote of: repo named: aRemote remoteName) 
			lookup; 
			fetchWithCredentials: IceCredentialsProvider default ]