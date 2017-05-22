fields description
fieldsDesc
	^#(
		LGitOptionsVersionsEnum version      "version for the struct"
		#FFIUInt32 flags            "defaults to GIT_DIFF_NORMAL"

		"options controlling which files are in the diff"

		LGitSubmoduleIgnoreTypeEnum ignore_submodules "submodule ignore rule"
		LGitStringArray       pathspec     "defaults to include all paths"
		LGitDiffNotifyCallback notify_cb
		#FFIVoid              * notify_payload

		"options controlling how to diff text is generated"

		#FFIUInt32    context_lines    "defaults to 3"
		#FFIUInt32    interhunk_lines  "defaults to 0"
		#FFIUInt16    id_abbrev       "default 'core.abbrev' or 7 if unset"
		#FFIInt64   max_size         "defaults to 512MB"
		#FFIExternalString old_prefix       "defaults to 'a'"
		#FFIExternalString new_prefix       "defaults to 'b'"
	)