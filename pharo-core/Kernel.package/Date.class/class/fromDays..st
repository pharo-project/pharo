instance creation
fromDays: dayCount 
	"Days since 1 January 1901"

	^ self julianDayNumber: SqueakEpoch + dayCount