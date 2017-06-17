operations
fetchFrom: remoteName 
	(LGitRemote of: self named: remoteName) 
		lookup; 
		fetch