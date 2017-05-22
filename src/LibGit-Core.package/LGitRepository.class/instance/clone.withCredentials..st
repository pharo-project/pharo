operations
clone: aString withCredentials: aCredentialsProvider 
	| cloneOptions |
	cloneOptions := LGitCloneOptions withCredentialsProvider: aCredentialsProvider.
	self
		clone: aString
		options: cloneOptions	
