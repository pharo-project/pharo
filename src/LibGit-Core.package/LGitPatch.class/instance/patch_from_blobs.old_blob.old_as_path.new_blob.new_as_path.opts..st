libgit-calls
patch_from_blobs: out old_blob: old_blob old_as_path: old_as_path new_blob: new_blob new_as_path: new_as_path opts: opts
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_patch_from_blobs #(LGitPatch * self , LGitBlob old_blob , String old_as_path , LGitBlob new_blob , String new_as_path , LGitDiffOptions * opts))
		options: #()