instance creation
starting: aDateAndTime duration: aDuration
	"Override - the duration is always one week.
	 Week will start from the Week class>>startDay"

	| midnight delta adjusted |
	midnight := aDateAndTime asDateAndTime midnight.
	delta := ((midnight dayOfWeek + 7 - (DayNames indexOf: self startDay)) rem: 7) abs.
	adjusted := midnight - (Duration days: delta seconds: 0).

	^ super
		starting: adjusted
		duration: (Duration weeks: 1).