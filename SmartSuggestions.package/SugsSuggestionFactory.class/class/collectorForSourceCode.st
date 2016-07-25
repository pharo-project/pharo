private
collectorForSourceCode 
	^ collectorForSourceCode ifNil: [collectorForSourceCode := self createCollector: #sourceCodeCommand].
