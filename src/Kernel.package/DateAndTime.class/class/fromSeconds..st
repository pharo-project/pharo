instance creation
fromSeconds: secondsSinceEpochUTC
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901 for the seconds in UTC time"
	^ self fromSeconds: secondsSinceEpochUTC offset: self localOffset