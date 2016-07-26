accessing
lastValue
	^ mutex critical: [ lastValue ]