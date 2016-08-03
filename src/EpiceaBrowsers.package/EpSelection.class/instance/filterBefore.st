operations
filterBefore

	self itemsForMenuAction ifNotEmpty: [ :items |
		| time |
		time := items first entry tags at: EpLog timeKey.
		self filterWith: (EpPluggableFilter before: time) ]