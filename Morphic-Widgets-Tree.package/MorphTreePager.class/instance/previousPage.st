accessing
previousPage
	self currentPageFirstIndex > 1
		ifTrue: [currentPageFirstIndex := (currentPageFirstIndex - pageSize) max: 1.
			self showCurrentPage]