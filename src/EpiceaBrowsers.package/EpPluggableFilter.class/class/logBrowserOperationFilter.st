well-known filters
logBrowserOperationFilter
	^ self
		title: 'Show only apply, revert or comment events'
		condition: [ :e | e content isKindOf: EpLogBrowserOperation ]