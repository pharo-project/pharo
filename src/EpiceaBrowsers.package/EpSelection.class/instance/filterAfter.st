operations
filterAfter

	self selectedEntryItems ifNotEmpty: [ :items |
		| time |
		time := items last entry tags at: EpLog timeKey.
		self filterWith: (EpPluggableFilter after: time) ]