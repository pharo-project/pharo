accessing - announcement info
announcementTimestamp
	^ self infoAt: #timestamp ifAbsent: [ self timestamp ]