accessing - announcement info
computerUUID
	^ self infoAt: #computerUUID ifAbsent: [ GlobalIdentifier uniqueInstance computerUUID asString ]