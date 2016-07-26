accessing
dayOfWeek
	"Sunday=1, ... , Saturday=7"

	^ (self julianDayNumber + 1 rem: 7) + 1