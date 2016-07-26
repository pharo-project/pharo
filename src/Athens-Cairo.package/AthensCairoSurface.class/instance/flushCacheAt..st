caching
flushCacheAt: anObject

	"Flush (delete) any cached value(s) identified by given object, anObject.
	Do nothing if there's no cached values stored for given object.
	Answer receiver.

	A surface using identity comparison for object identifiers.
	"
	CairoBackendCache soleInstance removeAt: anObject