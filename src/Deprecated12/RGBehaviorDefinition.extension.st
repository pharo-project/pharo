Extension { #name : 'RGBehaviorDefinition' }

{ #category : '*Deprecated12' }
RGBehaviorDefinition >> category [

	self deprecated: 'USe the concept of packages and package tags instead of the concept of categories.'.
	^ self package ifNotNil: [ :aPackage |
		  self packageTag
			  ifNil: [ aPackage name ]
			  ifNotNil: [ :tag | aPackage name , '-' , tag ] ]
]
