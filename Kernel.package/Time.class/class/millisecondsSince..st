accessing
millisecondsSince: lastTime
 	"Answer the elapsed time since last recorded in milliseconds.
 	Compensate for rollover."
 
 	^self milliseconds: self millisecondClockValue since: lastTime