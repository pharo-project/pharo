accessing
fieldsDesc
	^#(
		LGitOptionsVersionsEnum version;
		
		"
		 * These options are passed to the checkout step. To disable
		 * checkout, set the `checkout_strategy` to
		 * `GIT_CHECKOUT_NONE`.
		 "
		LGitCheckoutOptions checkout_opts;

		"
		 * Options which control the fetch, including callbacks.
		 *
		 * The callbacks are used for reporting fetch progress, and for acquiring
		 * credentials in the event they are needed.
		 "
		LGitFetchOptions fetch_opts;

		"
		 * Set to zero (false) to create a standard repo, or non-zero
		 * for a bare repo
		 "
		Boolean bare;

		"
		 * Whether to use a fetch or copy the object database.
		 "
		LGitCloneLocalTypeEnum local;

		"
		 * The name of the branch to checkout. NULL means use the
		 * remote's default branch.
		 "
		#FFIExternalString checkout_branch;

		"
		 * A callback used to create the new repository into which to
		 * clone. If NULL, the 'bare' field will be used to determine
		 * whether to create a bare repository.
		 "
		LGitRepositoryCreateCallback repository_cb;

		"
		 * An opaque payload to pass to the git_repository creation callback.
		 * This parameter is ignored unless repository_cb is non-NULL.
		 "
		#FFIVoid * repository_cb_payload;

		"
		 * A callback used to create the git_remote, prior to its being
		 * used to perform the clone operation. See the documentation for
		 * git_remote_create_cb for details. This parameter may be NULL,
		 * indicating that git_clone should provide default behavior.
		 "
		LGitRemoteCreateCallback remote_cb;

		"
		 * An opaque payload to pass to the git_remote creation callback.
		 * This parameter is ignored unless remote_cb is non-NULL.
		 "
		#FFIVoid * remote_cb_payload;
	)