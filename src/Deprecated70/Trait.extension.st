Extension { #name : #Trait }

{ #category : #'*Deprecated70' }
Trait >> baseTrait [
	
	self
		deprecated: 'Please use #instanceSide instead'
		transformWith: '`@receiver instanceSide' -> '`@receiver instanceSide'.

	^ self instanceSide 
]
