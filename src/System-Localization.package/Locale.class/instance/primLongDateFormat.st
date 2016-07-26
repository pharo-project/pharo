system primitives
primLongDateFormat
	"Returns the long date format
	d day, m month, y year,
	double symbol is null padded, single not padded (m=6, mm=06)
	dddd weekday
	mmmm month name"
	<primitive:'primitiveLongDateFormat' module: 'LocalePlugin'>
	^'dddd, mmmm d, yyyy'
