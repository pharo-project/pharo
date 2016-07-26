navigation
updateForNewPageSize: aPageSize
	pageSize := aPageSize max: 1.
	self changed: #pageSize