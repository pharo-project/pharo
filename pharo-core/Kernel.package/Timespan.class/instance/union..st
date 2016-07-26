enumerating
union: aTimespan
	 "Return the Timespan spanned by both"

	| aBegin anEnd |

	aBegin := self start min: aTimespan start.
	anEnd := self end max: aTimespan end.
	^ Timespan starting: aBegin ending: (anEnd + DateAndTime clockPrecision).
