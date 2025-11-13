Extension { #name : 'ReRefactoring' }

{ #category : '*Deprecated14' }
ReRefactoring >> asRefactoring [

	self deprecated: 'This method is a no-op and can be removed' transformWith: '`@rec asRefactoring' -> '`@rec'.
	^ self
]

{ #category : '*Deprecated14' }
ReRefactoring >> transform [ 

	self deprecated: 'Use privateTransform instead' transformWith: '`@rec transform' -> '`@rec privateTransform'
]
