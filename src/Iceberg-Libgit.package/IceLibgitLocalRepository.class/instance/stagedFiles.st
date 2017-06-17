actions
stagedFiles
	| statusMask modifiedPaths |
	statusMask :=  (LGitStatusTypeEnum git_status_index_new value)
				| (LGitStatusTypeEnum git_status_index_modified value)
				| (LGitStatusTypeEnum git_status_index_deleted value)
				| (LGitStatusTypeEnum git_status_index_typechange value)
				| (LGitStatusTypeEnum git_status_index_renamed value).
	
	self withRepoDo: 
		[ :repo | 
		modifiedPaths := repo index 
			entriesSelect: [ :path :status | (status & statusMask) > 0 ] ].
	^ modifiedPaths 
		collect: [ :entry | entry first asFileReference ]