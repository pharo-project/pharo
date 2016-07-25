accessing
julianDayOffset
	"Return the offset in julian days possibly introduced by the timezone offset"
	
	^ ((seconds + self offset asSeconds) / SecondsInDay) floor