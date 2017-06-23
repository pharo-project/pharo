actions
commitIndexWithMessage: message andParents: parentCommitishList

	self withRepoDo: [ :repo | 
		| signature commitId indexTreeId branchIsNew |
		[ signature := repo defaultSignature ]
		on: LGit_GIT_ENOTFOUND do: [ :e | 
			self 
				guessUsernameOrEmailNotFound: e 
				ifNot: [ e pass ] ].
		indexTreeId := repo index writeTreeAndReturnOid.		 
		commitId := (LGitCommitBuilder of: repo)
			tree: (LGitTree of: repo fromId: indexTreeId);
			message: message;
			parents: (repo isUnborn
				ifTrue: [ #() ]
				ifFalse: [ self libGitCommitsFrom: repo for: parentCommitishList ]);
			"'HEAD' branch could not exist yet. No problem, libgit will create it, 
			but we have to remember it to notify the frontend."
			updateReference: ([ branchIsNew := false. repo head name ] 
				on: LGit_GIT_EUNBORNBRANCH
				do: [ branchIsNew := true. 'refs/heads/', self branch nameForFirstCommit ]);
			author: signature;
			committer: signature;
			writeCommit;
			commitId.
		
			branchIsNew ifTrue: [ self branch: self lookupHead ].
			
		 ^ self class parseCommitInfo: (LGitCommit of: repo fromId: commitId).
	]
