accessing - announcement info
latestUpdate
	^ self infoAt: #latestUpdate ifAbsent: [ SystemVersion current highestUpdate ]