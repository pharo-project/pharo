private
collectorForMessage
	^ collectorForMessage ifNil: [collectorForMessage := self createCollector: #messageCommand].
