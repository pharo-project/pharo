accessing
seconds
 	"Answer a number that represents the number of complete seconds in the receiver, after the number of complete minutes has been removed."
	
	"1002003004 nanoSeconds seconds"
 
 	^ (seconds rem: SecondsInMinute)