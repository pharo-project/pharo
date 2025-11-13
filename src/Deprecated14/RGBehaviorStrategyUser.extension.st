Extension { #name : 'RGBehaviorStrategyUser' }

{ #category : '*Deprecated14' }
RGBehaviorStrategyUser >> theMetaClass [
	self
		deprecated: 'Please use #classSide instead'
		transformWith: '`@receiver theMetaClass' -> '`@receiver classSide'.

	^ self classSide
]

{ #category : '*Deprecated14' }
RGBehaviorStrategyUser >> theNonMetaClass [

	self
		deprecated: 'Please use #instanceSide instead'
		transformWith: '`@receiver theNonMetaClass' -> '`@receiver instanceSide'.
	^self instanceSide
]
