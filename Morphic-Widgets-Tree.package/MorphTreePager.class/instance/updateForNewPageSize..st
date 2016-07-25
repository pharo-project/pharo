user interface
updateForNewPageSize: newPageSize
	pageSize ~= newPageSize
		ifTrue: [ |  oldLast |
			oldLast := self currentPageLastIndex.
			pageSize := newPageSize max: 1.
			currentPageFirstIndex := (oldLast + 1 - pageSize) max: 1.
			self nodeList ifNotNil: [self showCurrentPage]]
