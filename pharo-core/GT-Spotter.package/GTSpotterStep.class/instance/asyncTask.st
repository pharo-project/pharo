accessing
asyncTask

	^ asyncTask ifNil: [ asyncTask := self defaultTask ]