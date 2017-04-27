special filters
after: aDateAndTime

	^ self
		title: 'after ', aDateAndTime printString
		condition: [ :e | (e tags at: EpLog timeKey) > aDateAndTime ]