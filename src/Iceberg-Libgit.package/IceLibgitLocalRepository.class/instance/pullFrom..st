actions
pullFrom: aRemote
	self withRepoDo: [ :repo | 
		| gitRemote myCommit theirCommit mergeBase |

		"Fetch"
		gitRemote := (LGitRemote of: repo named: aRemote remoteName) lookup.
		gitRemote fetchWithCredentials: IceCredentialsProvider default.		

		"Decide if we can fast-forward or we must do a real merge"
		myCommit := repo head object.
		theirCommit := repo head upstream object.
		mergeBase := repo mergeBaseBetween: myCommit id and: theirCommit id.
		
		(mergeBase = myCommit id)
			ifTrue: [ 
				"No need to merge, just fast-forward current HEAD"
				repo checkout_treeish: repo head upstream object.
				repo fastForward
			]
			ifFalse: [
				| mergedIndex treeId signature |
				
				"Merge both commits"
				mergedIndex := myCommit merge: theirCommit.
				
				"Commit index"
				signature := repo defaultSignature.
				treeId := mergedIndex writeTreeAndReturnOid.
	
				(LGitCommitBuilder of: repo)
					tree: (LGitTree of: repo fromId: treeId);
					message: 'Merge with ', repo head upstream name;
					parents: { myCommit. theirCommit };
					updateReference: repo head name;
					author: signature;
					committer: signature;
					writeCommit.		
			]
	]