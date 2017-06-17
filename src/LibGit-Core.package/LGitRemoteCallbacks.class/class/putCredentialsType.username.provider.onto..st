private
putCredentialsType: types username: username provider: provider onto: output  
	(types anyMask: 1 << 0) ifTrue: [ 
		^ LGitCredentialsPlaintext createFrom: provider onto: output ].
	(types anyMask: 1 << 1) ifTrue: [ 
		^ provider sshCredentialsClass 
			createFrom: provider 
			username: username
			onto: output ].

	Transcript logCr: ('I can''t create credentials for type: {1}' format: {types}).
	^ -1