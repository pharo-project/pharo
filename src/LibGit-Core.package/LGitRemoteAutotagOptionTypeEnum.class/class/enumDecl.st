enum declaration
enumDecl
	^ #(
		"
		 * Use the setting from the configuration.
		 "
		GIT_REMOTE_DOWNLOAD_TAGS_UNSPECIFIED 0
		"
		 * Ask the server for tags pointing to objects we're already
		 * downloading.
		 "
		GIT_REMOTE_DOWNLOAD_TAGS_AUTO 1
		"
		 * Don't ask for any tags beyond the refspecs.
		 "
		GIT_REMOTE_DOWNLOAD_TAGS_NONE 2
		"
		 * Ask for the all the tags.
		 "
		GIT_REMOTE_DOWNLOAD_TAGS_ALL 3
	)