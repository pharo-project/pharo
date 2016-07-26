caching
flushCacheAt: anObject

	"Flush (delete) any cached value(s) identified by given object, anObject.
	A surface using identity comparison for object identifiers.

	Answer receiver.
	"

	self subclassResponsibility