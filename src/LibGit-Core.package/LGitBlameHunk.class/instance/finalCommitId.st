accessing
finalCommitId
	"prim version doesn't work"
	"^ self prim_final_commit_id"
	
	^ LGitId fromHandle: (handle copyFrom: 3 to: 22)