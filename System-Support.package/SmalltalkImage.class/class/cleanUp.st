cleanup
cleanUp
	"Flush caches"
	
	"should probably distribute cleaning to systemDictionary"
	
	Smalltalk globals flushClassNameCache.
	Undeclared removeUnreferencedKeys