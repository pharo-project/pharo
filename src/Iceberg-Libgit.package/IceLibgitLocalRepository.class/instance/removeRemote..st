actions
removeRemote: aRemote
	self withRepoDo: [ :repo |
		(LGitRemote of: repo named: aRemote remoteName) 
			lookup;
			delete ]