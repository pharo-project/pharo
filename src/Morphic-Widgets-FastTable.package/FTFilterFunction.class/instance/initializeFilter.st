initialization
initializeFilter
	initialDataSource := table dataSource.
	isEditingSemaphore := Semaphore new.
	self spawnFilterUpdateThread