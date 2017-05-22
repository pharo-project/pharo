libgit-finalization
free
	"Free the memory referred to by the git_buf.

	Note that this does not free the git_buf itself, just the memory pointed to by buffer->ptr. This 	will not free the memory if it looks like it was not allocated internally, but it will clear the 	buffer back to the empty state."

	self class signature_free: handle