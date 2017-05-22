libgit-calls
diff_foreach: diff file_cb: file_cb binary_cb: binary_cb hunk_cb: hunk_cb line_cb: line_cb payload: payload
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_foreach #(self , LGitDiffFileCallback file_cb , LGitDiffBinaryCallback binary_cb , LGitDiffHunkCallback hunk_cb , LGitDiffLineCallback line_cb , void * payload))
		options: #(optMayGC optCoerceNilToNull)