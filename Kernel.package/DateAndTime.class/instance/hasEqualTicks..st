private
hasEqualTicks: aDateAndTime
	
	^ (self julianDayNumberUTC = aDateAndTime julianDayNumberUTC)
		and: [ (seconds = aDateAndTime secondsSinceMidnightUTC)
			and: [ nanos = aDateAndTime nanoSecond ] ]

