libgit-calls
object_free: handle 
	"We pass a handle in here instead of the object. We can't pass the object since that
	has already been gc'ed."
	^self call: #(void git_object_free(void *handle)) options: #(  )