private
ticks
	"Answer an array {days. seconds. nanoSeconds}. Used by DateAndTime and Time."

	| days |
	days := self days.
	^ Array 
		with: days
		with: seconds - (days * SecondsInDay)
		with: nanos