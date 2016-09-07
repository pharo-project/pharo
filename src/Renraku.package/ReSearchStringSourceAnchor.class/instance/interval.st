accessing
interval
	| start tweekedSubstring |
	
	tweekedSubstring := self searchString isSymbol
		ifTrue: [ self searchString keywords first ]
		ifFalse: [ self searchString ].
	
	start := (sourceEntity definition indexOfSubCollection: tweekedSubstring startingAt: 1).
	
	^ start to: start + tweekedSubstring size - 1