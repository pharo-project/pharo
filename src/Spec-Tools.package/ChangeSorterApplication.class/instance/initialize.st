initialization
initialize

	prettyPrint := false.
	showDiff := false.
	model := ChangeSorterModel new.
	SystemAnnouncer uniqueInstance weak
		when: CurrentChangeSetChanged send: #updateTitle to: self.
	super initialize.
	self initializeAnnouncements.
	
	
	