libgit-calls
diff_buffers: old_buffer old_len: old_len old_as_path: old_as_path new_buffer: new_buffer new_len: new_len new_as_path: new_as_path options: options file_cb: file_cb hunk_cb: hunk_cb line_cb: line_cb payload: payload
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_buffers #(FFIVoid * old_buffer , FFIUInt32 old_len , String old_as_path , FFIVoid * new_buffer , FFIUInt32 new_len , String new_as_path , LGitDiffOptions * options , LGitDiffFileCallback file_cb , LGitDiffHunkCallback hunk_cb , LGitDiffLineCallback line_cb , FFIVoid * payload))
		options: #(optMayGC optCoerceNilToNull)