private
collectorForUndeclaredVariable 
	^ collectorForUndeclaredVariable ifNil: [collectorForUndeclaredVariable := self createCollector: #undeclaredVarCommand].
