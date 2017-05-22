instance creation
createFrom: provider onto: lgitCredentials
	| credentials |
	[
		credentials := self new.
		provider providePlaintextCredentialsTo: credentials.
		credentials createWith: lgitCredentials.
		^ 0
	]
	on: LGitNoCredentialsProvided do: [ :e | 
		LGitError setError: 'Invalid username and/or password provided.'.				
		^ -1 ]
