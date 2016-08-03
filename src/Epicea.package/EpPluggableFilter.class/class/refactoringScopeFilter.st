well-known filters
refactoringScopeFilter
	^ self
		title: 'is refactoring'
		condition: [ :e | e content isKindOf: EpRefactoring ]