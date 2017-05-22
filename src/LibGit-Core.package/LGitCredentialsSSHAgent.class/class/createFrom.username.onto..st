instance creation
createFrom: provider username: username onto: lgitCredentials
	| credentials |
	
	[  
		credentials := self new.
		provider provideSshAgentCredentialsTo: credentials.	
		credentials createWith: username onto: lgitCredentials.
		^ 0 ]
	on: LGitNoCredentialsProvided do: [ 
		LGitError setError: 'No ssh-agent suitable credentials found.'.
		^ -1 ]