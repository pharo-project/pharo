instance creation
starting: startDateAndTime ending: endDateAndTime

	^ self 
		starting: startDateAndTime 
		duration: (endDateAndTime asDateAndTime - startDateAndTime). 