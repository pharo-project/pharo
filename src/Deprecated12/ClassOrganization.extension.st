Extension { #name : #ClassOrganization }

{ #category : #'*Deprecated12' }
ClassOrganization >> allCategories [

	self deprecated: 'Use #allProtocolsNames instead.' transformWith: '`@rcv allCategories' -> '`@rcv allProtocolsNames'.
	^ self allProtocolsNames
]

{ #category : #'*Deprecated12' }
ClassOrganization >> categories [

	self deprecated: 'Use #protocolsNames instead.' transformWith: '`@rcv categories' -> '`@rcv protocolsNames'.
	^ self protocolsNames
]
