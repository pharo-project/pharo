running
setUp
 	 "Schedule is a type of Timespan representing repeated occurences of the same event. 
	The beginning of the schedule is the first occurrence of the event.
	A schedule maintains an array of Durations. 
	Each durations specify the offset to the next scheduled each. 
	The duration of each occurence of the event is not specified. 
	Nor are any other attributes such as name"

	restoredTimeZone := DateAndTime localTimeZone.
	DateAndTime localTimeZone: (TimeZone timeZones detect: [:tz | tz abbreviation = 'GMT']).

	"Create aSchedule with an event scheduled for 8:30pm every Saturday 
	and Sunday for the year 2003. "
	"Create the first event occurring on the first Saturday at 8:30 pm: 1/4/03"
	firstEvent :=  DateAndTime year: 2003 month: 1 day: 4 hour: 20 minute: 30. 
	
	"Create a schedule for one year starting with the first event" 
	aSchedule := Schedule starting: firstEvent duration: 52 weeks.

	"Schedule the recurring events by scheduling the time in between each one.  
	One day for Sat-Sun. 6 days for Sun-Sat" 
	aSchedule schedule: { Duration days: 1. Duration days: 6 }.
