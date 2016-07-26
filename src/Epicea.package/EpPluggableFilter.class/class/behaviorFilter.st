well-known filters
behaviorFilter
	^ self
		title: 'class or trait'
		condition: [ :e | e content isEpBehaviorChange ]