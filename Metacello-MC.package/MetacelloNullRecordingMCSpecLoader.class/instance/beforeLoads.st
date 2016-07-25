accessing
beforeLoads

	beforeLoads == nil ifTrue: [ beforeLoads := OrderedCollection new ].
	^beforeLoads