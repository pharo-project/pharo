panes
pushPane: aMorph smooth: aBoolean

	self invariant.
	
	pagesCount := pagesCount + 1.
	
	self size = 1
		ifTrue: [ 
			firstVisiblePageIndex := 1.
			lastVisiblePageIndex := 1.
			selectedPageIndex := 1.
		]
		ifFalse: [ 
			(self actualNumberOfVisiblePages < self numberOfVisiblePages) ifTrue: [
				lastVisiblePageIndex := lastVisiblePageIndex + 1.
			].
		].
	
	self notifyPagePushed: aMorph scrollTo: true smooth: aBoolean.
	self size = 1 ifTrue: [ self notifySelectedPageChanged ].
	
	self invariant.