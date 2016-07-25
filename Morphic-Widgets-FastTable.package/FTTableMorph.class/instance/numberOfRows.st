accessing
numberOfRows
	self hasDataSource ifFalse: [ ^ 0 ]. 
	^ self dataSource numberOfRows