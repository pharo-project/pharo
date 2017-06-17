instance creation
createFrom: provider onto: lgitCredentials
	| credentials |
	
 	[
		credentials := self new.
		provider provideSshCredentialsTo: credentials.
		credentials createWith: lgitCredentials.
		^ 0
	]
	on: LGitNoCredentialsProvided do: [ 
		LGitError setError: 'Invalid ssh keys in provided files.'.		
		-1 ]
