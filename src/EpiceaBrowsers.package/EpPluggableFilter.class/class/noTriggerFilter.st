well-known filters
noTriggerFilter
	^ self
		title: 'Hide non-manual changes (e.g. changes produced by Monticello)'
		condition: [ :e | 
			e content isCodeChange and: [
			(e tags includesKey: EpLog triggererReferenceKey) not ] ]
