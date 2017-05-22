accessing
fieldsDesc
	^#(
		LGitOptionsVersionsEnum version;

		#FFIUInt32 checkout_strategy; "< default will be a dry run "

		#FFIInt32 disable_filters;    "< don't apply filters like CRLF conversion "
		#FFIUInt32 dir_mode;  "< default is 0755 "
		#FFIUInt32 file_mode; "< default is 0644 or 0755 as dictated by blob "
		#FFIInt32 file_open_flags;    "< default is O_CREAT | O_TRUNC | O_WRONLY "

		#FFIUInt32 notify_flags; "< see `git_checkout_notify_t` above "
		LGitCheckoutNotifyCallback notify_cb;
		#FFIVoid *  notify_payload;

		" Optional callback to notify the consumer of checkout progress. "
		LGitCheckoutProgressCallback progress_cb;
		#FFIVoid * progress_payload;

		" When not zeroed out, array of fnmatch patterns specifying which
		 *  paths should be taken into account, otherwise all files.  Use
		 *  GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH to treat as simple list.
		 "
		LGitStringArray paths;

		" The expected content of the working directory; defaults to HEAD.
		 *  If the working directory does not match this baseline information,
		 *  that will produce a checkout conflict.
		 "
		LGitTree * baseline;

		" Like `baseline` above, though expressed as an index.  This
		 *  option overrides `baseline`.
		 "
		LGitIndex * baseline_index; "< expected content of workdir, expressed as an index. "

		#FFIExternalString target_directory; "< alternative checkout path to workdir "

		#FFIExternalString ancestor_label; "< the name of the common ancestor side of conflicts "
		#FFIExternalString our_label; "< the name of the *our* side of conflicts "
		#FFIExternalString their_label; "< the name of the *their* side of conflicts "

		" Optional callback to notify the consumer of performance data. "
		LGitCheckoutPerfDataCallback perfdata_cb;
		#FFIVoid * perfdata_payload;
	)