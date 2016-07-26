accessing
hour12
	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
	of the day in the 12-hour clock of the local time of the receiver."
	
	^ self hour24 - 1 \\ 12 + 1