system primitives
primShortDateFormat
	"Returns the short date format
	d day, m month, y year,
	double symbol is null padded, single not padded (m=6, mm=06)
	dddd weekday
	mmmm month name"
	<primitive:'primitiveShortDateFormat' module: 'LocalePlugin'>
	^'m/d/yy'
