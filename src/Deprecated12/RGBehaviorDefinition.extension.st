Extension { #name : 'RGBehaviorDefinition' }

{ #category : '*Deprecated12' }
RGBehaviorDefinition >> category [

	self deprecated: 'USe the concept of packages and package tags instead of the concept of categories.'.
	^ self package ifNotNil: [ :aPackage |
		  self packageTag
			  ifNil: [ aPackage name ]
			  ifNotNil: [ :tag | aPackage name , '-' , tag ] ]
]

{ #category : '*Deprecated12' }
RGBehaviorDefinition >> theMetaClass [
	"This method is deprecated so consider to migrate."

	self
		deprecated: 'Please use #classSide instead'
		transformWith: '`@receiver theMetaClass' -> '`@receiver classSide'.

	^ self classSide
]

{ #category : '*Deprecated12' }
RGBehaviorDefinition >> theNonMetaClass [
	"This method is deprecated so consider to migrate."

	self
		deprecated: 'Please use #instanceSide instead'
		transformWith: '`@receiver theNonMetaClass' -> '`@receiver instanceSide'.

	^ self instanceSide
]
