libgit-calls
object_type: obj
	
	^ self
		call: #(LGitObjectTypeEnum git_object_type #(self))
		options: #()