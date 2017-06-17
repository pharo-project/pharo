libgit-calls
diff_blobs: old_blob old_as_path: old_as_path new_blob: new_blob new_as_path: new_as_path options: options file_cb: file_cb hunk_cb: hunk_cb line_cb: line_cb payload: payload
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_blobs #(LGitBlob old_blob , String old_as_path , LGitBlob new_blob , String new_as_path , LGitDiffOptions * options , LGitDiffFileCallback file_cb , LGitDiffHunkCallback hunk_cb , LGitDiffLineCallback line_cb , FFIVoid * payload))
		options: #(optMayGC optCoerceNilToNull)