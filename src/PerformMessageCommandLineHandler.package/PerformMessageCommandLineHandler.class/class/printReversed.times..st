testing
printReversed: aString times: aStringWithNumber

	aStringWithNumber asInteger timesRepeat: [ 
		Stdio stdout nextPutAll: aString reversed; lf. ]