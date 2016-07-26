private
sortByLastRecentlyUsed: lastRecentlyUsedEntries
	"Sort my entrys by the most recently used date"
	| lastRecentlyUsed |
	
	lastRecentlyUsed := Dictionary new.
	
	lastRecentlyUsedEntries do: [ :entry|
		lastRecentlyUsed at: entry contents put: entry date asSeconds ].
	
	entries sort: [ :a :b|| dateA dateB|
		dateA := lastRecentlyUsed at: a contents asString ifAbsent: [ 0 ].
		dateB := lastRecentlyUsed at: b contents asString ifAbsent: [ 0 ].
		dateA >= dateB].