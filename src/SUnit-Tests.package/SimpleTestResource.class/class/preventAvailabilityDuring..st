utility
preventAvailabilityDuring: aBlock
	"Only setter of preventAvailability."
	
	preventAvailability := false.
	^aBlock ensure: [preventAvailability := nil]