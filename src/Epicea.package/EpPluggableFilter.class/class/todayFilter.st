well-known filters
todayFilter
	^ self
		title: 'today'
		condition: [ :e | DateAndTime today < (e tags at: EpLog timeKey ifAbsent: [ DateAndTime epoch ])  ]