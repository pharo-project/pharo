libgit-calls
odb_write_pack: out db: db progress_cb: progress_cb progress_payload: progress_payload 
	^ self
		call: #(#LGitReturnCodeEnum git_odb_write_pack(LGitOdbWritepack * self, LGitOdb db, LGitTransferProgressCallback progress_cb, void * progress_payload))
		options: #( optMayGC optCoerceNilToNull )