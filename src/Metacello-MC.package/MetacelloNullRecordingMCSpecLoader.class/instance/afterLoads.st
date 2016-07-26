accessing
afterLoads

	afterLoads == nil ifTrue: [ afterLoads := OrderedCollection new ].
	^afterLoads