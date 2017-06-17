operations
merge: aLGitAnnotatedCommit 
	| heads |
	heads := (FFIExternalArray externalNewType: LGitAnnotatedCommit size: 1) autoRelease;
		at: 1 put: aLGitAnnotatedCommit handle;
		yourself.

	self withReturnHandlerDo: [
		self merge_heads: heads 
			length: 1 
			merge_options: LGitMergeOptions defaults
			checkout_options: LGitCheckoutOptions defaults 
	]
