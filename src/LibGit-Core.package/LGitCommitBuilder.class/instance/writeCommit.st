operations
writeCommit
	| parentsPointer |
	self withReturnHandlerDo: [ 
		self assert: (self parents allSatisfy: #isReady).
		parentsPointer := self parents
			ifEmpty: [ ExternalAddress null ]
			ifNotEmpty: [ (LGitExternalCommitArray fromCollection: self parents) getHandle ].
	self withReturnHandlerDo: [ 
		self
			commit_create: self commitId
			"libgit2 asserts the identity of this repository and the one it retrieves for the tree, so they need to be the same here"
			repo: self tree owner 
			update_ref: self updateReference
			author: self author
			committer: self committer
			message_encoding: self messageEncoding
			message: self message withUnixLineEndings
			tree: self tree
			parent_count: self parents size
			parents: parentsPointer] ]