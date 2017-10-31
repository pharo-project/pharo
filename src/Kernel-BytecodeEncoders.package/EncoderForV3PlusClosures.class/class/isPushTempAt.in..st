instruction stream support
isPushTempAt: pc in: method
	| byte |
	((byte := method at: pc) between: 16 and: 31) ifTrue: [ ^ true ].
	^ byte = 128 and: [ (method at: pc + 1) >> 6 = 1 ]