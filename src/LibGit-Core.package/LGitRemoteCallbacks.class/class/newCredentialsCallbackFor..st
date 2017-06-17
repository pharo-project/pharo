private
newCredentialsCallbackFor: provider
	^ LGitCredAcquireCallback
		on: [ :output :url :username_from_url :allowed_types :data | 
			"Allowed types are defined in git_credtype_t 
		https://github.com/libgit2/libgit2/blob/HEAD/include/git2/transport.h#L81-111"
			self
				putCredentialsType: allowed_types
				username: username_from_url
				provider: provider
				onto: output ]