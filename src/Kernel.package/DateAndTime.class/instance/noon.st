accessing
noon
	"Answer a DateAndTime starting at noon"

	^ self dayMonthYearDo: 
		[ :d :m :y | self class year: y month: m day: d hour: 12 minute: 0 second: 0 offset: offset]