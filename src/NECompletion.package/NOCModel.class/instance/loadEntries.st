private
loadEntries
	| priorityEntries  |
	"entries used from the last recently used"
	priorityEntries := 6.
	self loadNextModelEntries.
	
	entries size <= priorityEntries
		ifTrue: [ ^ self ].
		
	"most recently used entries first!"
	self sortByLastRecentlyUsed: self loadFavoredEntries.
		
	"sort the rest alphabetically"
	entries 
		mergeSortFrom: priorityEntries
		to: entries size 
		by: [ :a :b| 
			(a contents asString compare: b contents asString caseSensitive: false) <= 2 ]