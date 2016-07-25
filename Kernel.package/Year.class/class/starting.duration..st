instance creation
starting: aDateAndTime duration: aDuration 
	"Return a new year, starting from the beginning the year the given DateAndTime is in."
	| aYear |
	aYear := aDateAndTime asDateAndTime year.

	^ super
		starting: (DateAndTime year: aYear month: 1 day: 1)
		duration: (Duration days: (self daysInYear: aYear)).