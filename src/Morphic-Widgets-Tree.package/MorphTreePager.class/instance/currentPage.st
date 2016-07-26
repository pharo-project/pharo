accessing
currentPage
	^ self currentPageLastIndex \\ pageSize > 0
		ifTrue: [(self currentPageLastIndex / pageSize) asInteger + 1]
		ifFalse: [(self currentPageLastIndex / pageSize) asInteger]