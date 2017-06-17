fields description
fieldsDesc
	^#(
		LGitTransferProgress * stats;
		LGitTransferProgressCallback * progress_cb;
		#FFIVoid * progress_payload;
		LGitOdbWritepack * writepack;
	)