well-known filters
lastSixHoursFilter
	^ self
		title: 'last 2 hours'
		condition: [ :e | (e tags at: EpLog timeKey ifAbsent: [ DateAndTime epoch ]) > (DateAndTime now - 2 hours) ]