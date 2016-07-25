accessing
withStartStopAndValueDo: aBlock
	| start |
	start := 1.
	runs with: values do:
		[:len : val | | stop |
		stop := start + len - 1.
		aBlock value: start value: stop value: val.
		start := stop + 1]
		