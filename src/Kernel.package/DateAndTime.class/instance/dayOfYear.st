accessing
dayOfYear
	"This code was contributed by Dan Ingalls. It is equivalent to the terser
		^ jdn - (Year year: self year) start julianDayNumber + 1 but much quicker."

	
	^ self dayMonthYearDo:
		[ :d :m :y | | monthStart |
			monthStart := #(1 32 60 91 121 152 182 213 244 274 305 335) at: m.
			(m > 2 and: [ Year isLeapYear: y ])
				ifTrue: [ monthStart + d ]
				ifFalse: [ monthStart + d - 1 ]]