Extension { #name : 'RGBehaviorStrategy' }

{ #category : '*Deprecated14' }
RGBehaviorStrategy >> theMetaClass [
	"This method is deprecated so consider to migrate."

	self
		deprecated: 'Please use #classSide instead'
		transformWith: '`@receiver theMetaClass' -> '`@receiver classSide'.

	^ self classSide
]

{ #category : '*Deprecated14' }
RGBehaviorStrategy >> theNonMetaClass [
	"This method is deprecated so consider to migrate."

	self
		deprecated: 'Please use #instanceSide instead'
		transformWith: '`@receiver theNonMetaClass' -> '`@receiver instanceSide'.

	^ self instanceSide
]
