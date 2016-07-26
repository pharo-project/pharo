well-known filters
logBrowserOperationFilter
	^ self
		title: 'undo/redo/comment'
		condition: [ :e | e content isKindOf: EpLogBrowserOperation ]