accessing
hour24
	"Answer a number that represents the number of complete hours in the receiver's time part,
	 after the number of complete days has been removed."
 
 	^ self localSeconds // SecondsInHour \\ 24