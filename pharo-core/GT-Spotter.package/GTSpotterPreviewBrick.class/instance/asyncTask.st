accessing
asyncTask

	^ asyncTask ifNil: [ asyncTask := GLMAsyncTask new ]