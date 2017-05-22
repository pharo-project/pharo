accessing
remotes
	| gitRemotes |
	self withRepoDo: [ :repo | gitRemotes := repo allRemotes ].
	^ gitRemotes collect: [ :each | 
		(IceRemote url: each url)
			remoteName: each remoteName;
			yourself ]