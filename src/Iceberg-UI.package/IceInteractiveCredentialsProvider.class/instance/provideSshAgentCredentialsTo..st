actions
provideSshAgentCredentialsTo: credentials
	"Just check for tries to ensure we do not enter an infinite cycle in libgit2"
	tries := tries + 1.

	tries >= 3 ifTrue: [ 
		Transcript show: 'aborting'; cr.
		LGitNoCredentialsProvided signal: 'No ssh-agent credentials found.' ].
