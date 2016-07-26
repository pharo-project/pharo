converting
asyncTaskUniqueInstance

	^ asyncTask ifNil: [ asyncTask := GLMAsyncTask new ]