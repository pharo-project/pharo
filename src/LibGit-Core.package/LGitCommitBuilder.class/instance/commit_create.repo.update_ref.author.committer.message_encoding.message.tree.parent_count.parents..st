libgit-calls
commit_create: theCommitId repo: repo update_ref: update_ref author: theAuthor committer: theCommitter message_encoding: message_encoding message: theMessage tree: theTree parent_count: parent_count parents: parentsPointer
	
	<todo:
		'try to use original function signature instead of passing ExternalAddress as last arg'>
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_commit_create #(LGitId * theCommitId , LGitRepository repo , String update_ref , LGitSignature * theAuthor , LGitSignature * theCommitter , String message_encoding , String theMessage , LGitTree theTree , int parent_count , ExternalAddress parentsPointer))
		options: #(optCoerceNilToNull)