private
collectorForTemporaryVariable 
	^ collectorForTemporaryVariable  ifNil: [collectorForTemporaryVariable := self createCollector: #tempVarCommand].
	