helpers
useNonUtcTimeZoneDuring: aBlock

	self useTimeZone: 'EDT' during: aBlock.