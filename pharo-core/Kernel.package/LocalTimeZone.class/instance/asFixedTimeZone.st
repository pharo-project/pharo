converting
asFixedTimeZone
	"Convert this dynamic timezone to one with a fixed offset."
	^ TimeZone
		offset: self offset
		name: 'Fixed Local Time'
		abbreviation: self abbreviation