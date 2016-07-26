accessing
minute
	"Answer a number that represents the number of complete minutes in the receiver' time part, 
	after the number of complete hours has been removed."
	
 	^ self localSeconds // SecondsInMinute \\ 60