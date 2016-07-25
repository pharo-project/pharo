initialize
timeToLive: duration
	"Set how old computed values can be before they become stale.
	When stale, a value will be computed again based on the block passed in.
	Specify either an Integer or a Duration, both will be converted to seconds."
	
	timeToLive := duration asDuration asSeconds