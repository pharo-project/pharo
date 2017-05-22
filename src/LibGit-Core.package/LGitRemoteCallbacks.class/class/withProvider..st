instance creation
withProvider: provider

	^ LGitRemoteCallbacks defaults
		credentials: (self newCredentialsCallbackFor: provider);
		certificateCheck: self newCertificateCheckCallback;
		yourself