structure
topRendererOrSelf
	^ owner ifNil: [ super topRendererOrSelf ]