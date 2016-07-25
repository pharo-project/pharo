tests
testExampleFromSwikiPage
	"It is often neccessary to schedule repeated events, like airline flight schedules, TV programmes, and file backups.
	 Schedule is a Timespan which maintains an array of Durations.
	 The durations specify the offset to the next scheduled DateAndTime. "
	"Consider a TV programme scheduled for 8:30pm every Saturday and Sunday for the current year. "
	"Find the first Saturday and set its time to 20h30"
	| sat shows |
	sat := Year current asMonth dates detect: [ :d | d dayOfWeekName = #Saturday ].
	sat := sat start + (Duration hours: 20.5).
	"Create a schedule" 
	shows := Schedule starting: sat ending: Year current end.
	shows schedule: { Duration days: 1. Duration days: 6 }.
	"To inspect:"
	shows dateAndTimes.
	shows dateAndTimes collect: [ :dt | dt dayOfWeekName ].

