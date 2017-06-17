initialize
annotated_commit_from_ref: ref
	^ self callUnchecked: #(
		int git_annotated_commit_from_ref #(
			LGitAnnotatedCommit * self , 
			LGitRepository repository ,
			LGitReference ref)) 
		options: #()