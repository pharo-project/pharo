Extension { #name : #ClassOrganization }

{ #category : #'*Deprecated12' }
ClassOrganization >> addCategory: aString [

	self deprecated: 'Use #addProtocolNamed: instead.' transformWith: '`@rcv addCategory: `@arg' -> '`@rcv addProtocolNamed: `@arg'.
	self addProtocolNamed: aString
]

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
