accessing
fieldsDesc
	^#(
		LGitStructVersionsEnum version;
		"
		 * Textual progress from the remote. Text send over the
		 * progress side-band will be passed to this function (this is
		 * the 'counting objects' output.
		 "
		LGitTransportMessageCallback sideband_progress;

		"
		 * Completion is called when different parts of the download
		 * process are done (currently unused).
		 "
		LGitCompletionCallback completion;

		"
		 * This will be called if the remote host requires
		 * authentication in order to connect to it.
		 *
		 * Returning GIT_PASSTHROUGH will make libgit2 behave as
		 * though this field isn't set.
		 "
		LGitCredAcquireCallback credentials;

		"
		 * If cert verification fails, this will be called to let the
		 * user make the final decision of whether to allow the
		 * connection to proceed. Returns 1 to allow the connection, 0
		 * to disallow it or a negative value to indicate an error.
		 "
	   LGitTransportCertificateCheckCallback certificate_check;

		"
		 * During the download of new data, this will be regularly
		 * called with the current count of progress done by the
		 * indexer.
		 "
		LGitTransferProgressCallback transfer_progress;

		"
		 * Each time a reference is updated locally, this function
		 * will be called with information about it.
		 "
		LGitUpdateTipsCallback update_tips;

		"
		 * Function to call with progress information during pack
		 * building. Be aware that this is called inline with pack
		 * building operations, so performance may be affected.
		 "
		LGitPackbuilderProgressCallback pack_progress;

		"
		 * Function to call with progress information during the
		 * upload portion of a push. Be aware that this is called
		 * inline with pack building operations, so performance may be
		 * affected.
		 "
		LGitPushTransferCallback push_transfer_progress;

		"
		 * Called for each updated reference on push. If `status` is
		 * not `NULL`, the update was rejected by the remote server
		 * and `status` contains the reason given.
		 "
		LGitPushUpdateReferenceCallback push_update_reference;

		"
		 * Called once between the negotiation step and the upload. It
		 * provides information about what updates will be performed.
		 "
		LGitPushNegotiationCallback push_negotiation;

		"
		 * Create the transport to use for this operation. Leave NULL
		 * to auto-detect.
		 "
		LGitTransport transport;

		"
		 * This will be passed to each of the callbacks in this struct
		 * as the last parameter.
		 "
		#FFIVoid * payload;
	)