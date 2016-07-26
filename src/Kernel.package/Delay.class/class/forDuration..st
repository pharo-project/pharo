instance creation
forDuration: aDuration
 	"Return a new Delay for the given duration."
 	^ self forMilliseconds: aDuration asMilliSeconds
 