tests
testRepositoriesIncludeCache
	"At least one cache is present in the list of repositories"
	self assert: (b repositories anySatisfy: [ :r | r class == MCCacheRepository ])