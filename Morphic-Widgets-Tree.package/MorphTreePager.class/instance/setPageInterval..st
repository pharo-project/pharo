accessing
setPageInterval: anInterval
	currentPageFirstIndex := anInterval first.
	self showCurrentPage.
	self changed: #currentPage