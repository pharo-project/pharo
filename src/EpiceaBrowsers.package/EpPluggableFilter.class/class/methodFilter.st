well-known filters
methodFilter
	^ self
		title: 'Show only changes to methods'
		condition: [ :e | e content isEpMethodChange ]