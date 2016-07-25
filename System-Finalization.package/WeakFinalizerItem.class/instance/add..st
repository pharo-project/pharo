accessing
add: newExecutor

	executor 
		ifNil: [ executor := newExecutor ]
		ifNotNil: [
			executor hasMultipleExecutors
				ifTrue: [ executor add: newExecutor]
				ifFalse: [ executor := ObjectFinalizerCollection with: executor with: newExecutor ]
		]