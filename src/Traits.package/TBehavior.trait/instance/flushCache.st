private
flushCache
	"Tell the virtual machine to remove the contents of its method lookup caches, if it has any.  This must be done when the system 	modifies the class hierarchy so that message lookups reflect the revised organization.  c.f. Symbol>>flushCache & 	CompiledMethod>>flushCache.  Essential. See MethodDictionary class comment."

	<primitive: 89>
	self primitiveFailed