initialization
initializeUpdateTask

	updateTask := GLMAsyncTask new
		priority: Processor userBackgroundPriority;
		yourself.