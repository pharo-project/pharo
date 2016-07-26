*UnifiedFFI
replaceFrom: start to: stop with: replacement startingAt: repStart 
	| dstAddress srcAddress repSize |
	
	self flag: #todo. "Maybe replace it with a primitive (like the one at Alien)?"
	repSize := stop - start + 1.
	dstAddress := start = 1 
		ifTrue: [ self ] 
		ifFalse: [ self + (start - 1) ].
	srcAddress := repStart = 1 
		ifTrue: [ replacement ] 
		ifFalse: [ replacement copyFrom: repStart to: repStart + repSize ].	
	LibC memCopy: srcAddress to: dstAddress size: repSize