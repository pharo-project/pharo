accessing
dayOfMonth
	"Answer which day of the month is represented by the receiver."

	^ self dayMonthYearDo: [ :d :m :y | d ]