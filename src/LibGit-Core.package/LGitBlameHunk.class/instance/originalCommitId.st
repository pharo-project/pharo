accessing
originalCommitId
	"prim version doesn't work"
	"^ self prim_orig_commit_id"
	
	^LGitId fromHandle: (handle copyFrom: 29 to: 48)