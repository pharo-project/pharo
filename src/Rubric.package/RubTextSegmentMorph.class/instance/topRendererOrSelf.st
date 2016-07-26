structure
topRendererOrSelf
	^owner
		ifNotNil: [ owner topRendererOrSelf ]