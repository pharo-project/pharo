printing
removeAll
	"See super"
	
	self protected:[
		valueDictionary removeAll.
		list swapWithNil. "prune finalization list as well"
	].