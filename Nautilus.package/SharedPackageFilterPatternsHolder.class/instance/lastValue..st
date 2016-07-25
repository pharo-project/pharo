accessing
lastValue: aValue 
	mutex critical: [ lastValue := aValue ]