instance creation
withCredentialsCallback: aBlock
	| credentialsCallback callbacks fetchOptions  |
	credentialsCallback := LGitCredAcquireCallback on: aBlock.

	callbacks := LGitRemoteCallbacks defaults
		credentials: credentialsCallback;
		yourself.

	fetchOptions := LGitFetchOptions defaults
		callbacks: callbacks;
		yourself.

	^ self defaults
		fetchOptions: fetchOptions;
		yourself.
	
