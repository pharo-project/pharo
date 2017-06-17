actions
storeVersion: aMCVersion 
	self withRepoDo: [ :repo | 
		| treeBuilder signature parentCommit head |
		head := repo head.
		parentCommit := head object.
		treeBuilder := IceLibgitTreeBuilder on: parentCommit tree.
		IceLibgitWriter fileOut: aMCVersion on: (treeBuilder entryByPath: self subdirectory).
		
		signature := repo defaultSignature.

		(LGitCommitBuilder of: repo)
			tree: treeBuilder buildTree;
			message: aMCVersion info message;
			parents: { parentCommit };
			updateReference: head;
			author: signature;
			committer: signature;
			writeCommit.		
	]
