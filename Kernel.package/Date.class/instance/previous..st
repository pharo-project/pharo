accessing
previous: dayName 
	"Answer the previous date whose weekday name is dayName."

	| days |
	days := 7 + self weekdayIndex - (self class dayOfWeek: dayName) \\ 7.
	days = 0 ifTrue: [ days := 7 ].
	^ self subtractDays: days
