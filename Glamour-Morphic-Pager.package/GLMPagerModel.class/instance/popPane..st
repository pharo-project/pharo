panes
popPane: isSmooth

	self invariant.
	
	pagesCount >= 1 ifTrue: [
		|scrollTo smooth|
		smooth := isSmooth.
		
		focusedMorphs removeKey: pagesCount  ifAbsent: [ true ].
		pagesCount := pagesCount - 1.
		
		firstVisiblePageIndex = 1 ifTrue: [ smooth := false ].
		
		lastVisiblePageIndex > pagesCount ifTrue: [
			firstVisiblePageIndex := firstVisiblePageIndex - 1.
			lastVisiblePageIndex := lastVisiblePageIndex - 1.
		].
	
		firstVisiblePageIndex := firstVisiblePageIndex max: 1.
	
		pagesCount = 0 ifTrue: [
			selectedPageIndex := 0.
			firstVisiblePageIndex := 0.
		].
		
		self selectedPageIndex > pagesCount ifTrue: [
			self selectedPageIndex: pagesCount.
		].
	
		scrollTo := ((pagesCount = firstVisiblePageIndex) & (pagesCount > self numberOfVisiblePages)).
		self notifyPagePoped: smooth.
	].

	self invariant.