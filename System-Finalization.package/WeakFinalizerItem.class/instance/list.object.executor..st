initialize-release
list: weakFinalizationList object: anObject executor: anExecutor
	self assert: (weakFinalizationList class == WeakFinalizationList).
	list := weakFinalizationList.
	self at: 1 put: anObject.
	executor := anExecutor