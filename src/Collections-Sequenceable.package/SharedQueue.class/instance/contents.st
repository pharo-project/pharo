accessing
contents
	" To be compliant with stream API but is really needed? "
	^ monitor critical:
		[ items copy ]