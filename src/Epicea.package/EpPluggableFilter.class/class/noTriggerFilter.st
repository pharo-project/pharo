well-known filters
noTriggerFilter
	^ self
		title: 'is manual change'
		condition: [ :e | (e tags includesKey: EpLog triggererReferenceKey) not ]