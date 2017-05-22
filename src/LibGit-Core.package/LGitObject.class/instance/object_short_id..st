libgit-calls
object_short_id: out
	
	^ self
		call: #(LGitObjectTypeEnum git_object_short_id(LGitBuf *out, self))
		options: #()