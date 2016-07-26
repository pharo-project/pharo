operations
roundTo: aDuration
 	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 6 minutes."
 
 	^ self class nanoSeconds: (self asNanoSeconds roundTo: aDuration asNanoSeconds)
 
 