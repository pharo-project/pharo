instance creation
withCredentialsProvider: provider
	| fetchOptions |
	fetchOptions := LGitFetchOptions defaults
		callbacks: (LGitRemoteCallbacks withProvider: provider);
		yourself.

	^ self defaults
		fetchOptions: fetchOptions
		yourself.
	
