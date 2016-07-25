accessing
nextPage
	self currentPageLastIndex < self nodeList size
		ifTrue: [currentPageFirstIndex := (currentPageFirstIndex + pageSize) min: self nodeList size.
			self showCurrentPage]