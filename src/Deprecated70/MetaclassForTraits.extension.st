Extension { #name : #MetaclassForTraits }

{ #category : #'*Deprecated70' }
MetaclassForTraits >> baseTrait [
	
	self
		deprecated: 'Please use #instanceSide instead'
		transformWith: '`@receiver instanceSide' -> '`@receiver instanceSide'.

	^ self instanceSide 
]
