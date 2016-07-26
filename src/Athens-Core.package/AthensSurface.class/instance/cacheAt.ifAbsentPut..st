caching
cacheAt: anObject ifAbsentPut: aBlock
	"Answer an object from surface's cache identified by anObject,
	if there is no cached object under such identifier, evaluate a block
	and put it into cache. Then answer the result of evaluation.
	A surface using identity comparison for object identifiers.
	"

	self subclassResponsibility