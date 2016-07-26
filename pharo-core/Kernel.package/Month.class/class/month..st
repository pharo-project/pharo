instance creation
month: month
	"Create a Month in the current year for the given <month>.
	<month> may be a number or a String with the name of the month."

	^ self 
		year: DateAndTime now year
		month: month
