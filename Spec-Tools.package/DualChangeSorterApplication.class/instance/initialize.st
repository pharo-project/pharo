initialization
initialize

	super initialize.
	model := ChangeSorterModel new.
	isRefreshing := false.
	SystemAnnouncer uniqueInstance weak 
		when: CurrentChangeSetChanged 
		send: #updateTitle 
		to: self 