special filters
before: aDateAndTime

	^ self
		title: 'before ', aDateAndTime printString
		condition: [ :e | (e tags at: EpLog timeKey) < aDateAndTime ]