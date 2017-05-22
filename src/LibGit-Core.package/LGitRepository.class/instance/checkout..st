operations
checkout: aString
	<todo: 'use proper exceptions'>
	self assert: aString notNil.
	self assert: aString trimBoth notEmpty.

	[ 
		| branch |
		branch := self lookupLocalBranch: aString ifAbsent: [ self createBranchFromOrigin: aString ].

		self checkout_treeish: branch object.
		self setHead: branch.
	] 
	on: LGit_GIT_EINVALIDSPEC, LGit_GIT_ENOTFOUND do: [
		"aString is not a valid branch name, so try to find a corresponding treeish using revparse, 
		this will handle stuff like 'master~1'"
		| commitish | 
		commitish := self revparse: aString.
		self checkout_treeish: commitish.
		self setHeadDetached: commitish.
	].

