libgit-calls
object_id: obj
	
	^ self
		call: #(void * git_object_id #(self))
		options: #()