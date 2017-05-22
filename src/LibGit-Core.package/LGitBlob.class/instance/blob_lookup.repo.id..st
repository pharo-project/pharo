libgit-calls
blob_lookup: blob repo: repo id: objectId
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_blob_lookup #(LGitBlob * blob , LGitRepository repo , LGitId * objectId))
		options: #()