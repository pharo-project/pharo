libgit-calls
object_dup: dest from: source
	
	^ self
		callUnchecked: #(LGitObjectTypeEnum git_object_dup(LGitObject *dest, LGitObject source))
		options: #()