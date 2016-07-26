private
destroyHandle
	"remove receiver from finalization registry, so #finalize won't try to free handle again" 
	self class deregister: self. 

	self pvtDestroyHandle.
