Extension { #name : 'RGBehaviorStrategyUser' }

{ #category : '*Deprecated14' }
RGBehaviorStrategyUser >> theMetaClass [
	self
		deprecated: 'Please use #classSide instead'
		in: #Pharo14
		transformWith: '`@receiver theMetaClass' -> '`@receiver classSide'.

	^ self classSide
]

{ #category : '*Deprecated14' }
RGBehaviorStrategyUser >> theNonMetaClass [

	self
		deprecated: 'Please use #instanceSide instead'
		in: #Pharo14
		transformWith: '`@receiver theNonMetaClass' -> '`@receiver instanceSide'.
	^self instanceSide
]
