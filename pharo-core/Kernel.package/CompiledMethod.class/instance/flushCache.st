accessing
flushCache
	"Tell the virtual machine to remove all references to this method from its method lookup caches, and to discard any optimized version 	of the method, if it has any of these.  This must be done whenever a method is modified in place, such as modifying its literals or 	machine code, to reflect the revised code.  c.f. Behavior>>flushCache & Symbol>>flushCache.  Essential.	 See MethodDictionary class 	comment."

	<primitive: 116>
