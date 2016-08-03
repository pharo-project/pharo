well-known filters
behaviorFilter
	^ self
		title: 'is class or trait change'
		condition: [ :e | e content isEpBehaviorChange ]