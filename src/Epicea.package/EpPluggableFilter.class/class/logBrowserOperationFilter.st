well-known filters
logBrowserOperationFilter
	^ self
		title: 'is apply, revert or comment'
		condition: [ :e | e content isKindOf: EpLogBrowserOperation ]