accessing
hours
 	"Answer a number that represents the number of complete hours in the receiver, after the number of complete days has been removed."
 
 	^ (seconds rem: SecondsInDay) quo: SecondsInHour
 