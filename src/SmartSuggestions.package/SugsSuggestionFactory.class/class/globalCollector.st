private
globalCollector
	^ collectorForAll ifNil: [collectorForAll  := self createCollector: #globalCommand].
