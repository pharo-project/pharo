libgit-calls
object_free: handle 
	^self call: #(void git_object_free(void *handle)) options: #(  )