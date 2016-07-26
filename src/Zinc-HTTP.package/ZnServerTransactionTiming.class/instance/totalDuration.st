accessing
totalDuration
	"requestDuration is unreliable because it contains idle wait time"
	
	^ handlerDuration + responseDuration 