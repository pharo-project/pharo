filtering
accepts: anEntry
	^ self subFilters allSatisfy: [ :aFilter | aFilter accepts: anEntry ]