accessing
interval
	| start tweekedSubstring |
	
	tweekedSubstring := searchString isSymbol
		ifTrue: [ searchString keywords first ]
		ifFalse: [ searchString ].
	
	start := (sourceEntity definition indexOfSubCollection: tweekedSubstring startingAt: 1).
	
	^ start to: start + tweekedSubstring size - 1