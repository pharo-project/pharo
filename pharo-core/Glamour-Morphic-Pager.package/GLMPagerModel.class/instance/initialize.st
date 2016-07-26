initialization
initialize
	super initialize.

	pagesCount := 0.
	selectedPageIndex := 0.
	focusedMorphs := Dictionary new.
	isDependenciesShown := false.
		
	numberOfVisiblePages := self defaultNumberOfVisiblePages.
	
	firstVisiblePageIndex:= 0.
	lastVisiblePageIndex:= 0.
	
	self invariant.