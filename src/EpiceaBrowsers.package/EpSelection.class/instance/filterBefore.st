operations
filterBefore

	self selectedEntryItems ifNotEmpty: [ :items |
		| time |
		time := items first entry tags at: EpLog timeKey.
		self filterWith: (EpPluggableFilter before: time) ]