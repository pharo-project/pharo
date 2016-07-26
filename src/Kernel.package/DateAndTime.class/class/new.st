instance creation
new
	"Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

	^ self epoch offset: self localOffset 
	