accessing
compilationContext
	^ compilationContext ifNil: [ compilationContext := self compilationContextClass default ]