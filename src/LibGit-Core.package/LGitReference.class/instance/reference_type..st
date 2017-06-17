libgit-calls
reference_type: ref
	
	^ self
		call: #(LGitReferenceTypeEnum git_reference_type #(self))
		options: #()