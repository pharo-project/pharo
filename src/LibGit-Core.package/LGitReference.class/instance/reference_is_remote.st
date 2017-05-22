libgit-calls
reference_is_remote
	^ self call: #(Boolean git_reference_is_remote #(self)) options: #()