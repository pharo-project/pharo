private
collectorForClass 
	^ collectorForClass ifNil: [collectorForClass := self createCollector: #classCommand].
