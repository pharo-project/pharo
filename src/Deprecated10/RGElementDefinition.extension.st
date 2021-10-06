Extension { #name : #RGElementDefinition }

{ #category : #'*Deprecated10' }
RGElementDefinition >> theNonMetaParentName [

	self
		deprecated: 'Please use #instanceSideParentName instead'
		transformWith: '`@receiver theNonMetaParentName' -> '`@receiver instanceSideParentName'.
	^ self instanceSideParentName
]
