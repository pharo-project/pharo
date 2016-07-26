structure
topRendererOrSelf

	^ scrollPane 
		ifNil: [ super topRendererOrSelf ]
		ifNotNil: [ scrollPane topRendererOrSelf ]