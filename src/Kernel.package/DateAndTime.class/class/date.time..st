instance creation
date: aDate time: aTime

	^ self 
		year: aDate year
		month: aDate monthName
		day: aDate dayOfMonth 
		hour: aTime hour 
		minute: aTime minute 
		second: aTime second
		nanoSecond: aTime nanoSecond
		offset: self localOffset.