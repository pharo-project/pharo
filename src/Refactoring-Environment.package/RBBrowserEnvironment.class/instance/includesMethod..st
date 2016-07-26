testing
includesMethod: aMethod
	^ self 
		includesSelector: aMethod selector
		in: aMethod methodClass