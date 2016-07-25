private
collectorForMethod 
	^ collectorForMethod ifNil: [collectorForMethod := self createCollector: #methodCommand].
