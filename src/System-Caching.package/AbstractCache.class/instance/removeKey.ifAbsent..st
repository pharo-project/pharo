removing
removeKey: key ifAbsent: block
	"If I currently cache key, remove the entry.
	Execute block when key is currently absent.
	Return the removed value."
	
	self subclassResponsibility 