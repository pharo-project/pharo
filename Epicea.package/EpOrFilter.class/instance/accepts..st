filtering
accepts: anEntry
	^ self subFilters anySatisfy: [ :aFilter | aFilter accepts: anEntry ]