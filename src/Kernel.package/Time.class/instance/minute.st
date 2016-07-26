accessing
minute
	"Answer a number that represents the number of complete minutes in the receiver,
	after the number of complete hours has been removed."
 
	^ (seconds rem: SecondsInHour) quo: SecondsInMinute 