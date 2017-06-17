fields description
fieldsDesc
	^#(
		"
		 * The source name of the reference
		 "
		#FFIExternalString src_refname;
		"
		 * The name of the reference to update on the server
		 "
		#FFIExternalString dst_refname;
		"
		 * The current target of the reference
		 "
		LGitId src
		"
		 * The new target for the reference
		 "
		LGitId dst
	)