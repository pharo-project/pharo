Extension { #name : #ClassOrganization }

{ #category : #'*Deprecated12' }
ClassOrganization >> allCategories [

	self deprecated: 'Use #allProtocolsNames instead.' transformWith: '`@rcv allCategories' -> '`@rcv allProtocolsNames'.
	^ self allProtocolsNames
]
