system primitives
primTimeFormat
	"Returns string time format
	Format is made up of 
	h hour (h 12, H 24), m minute, s seconds, x (am/pm String)
	double symbol is null padded, single not padded (h=6, hh=06)"
	<primitive:'primitiveTimeFormat' module: 'LocalePlugin'>
	^'h:mmx'
