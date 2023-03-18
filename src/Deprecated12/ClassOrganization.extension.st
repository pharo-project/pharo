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

{ #category : #'*Deprecated12' }
ClassOrganization >> categoryOfElement: aSelector [

	self deprecated: 'Use #protocolNameOfElement: instead.' transformWith: '`@rcv categoryOfElement: `@arg' -> '`@rcv protocolNameOfElement: `@arg'.
	^ self protocolNameOfElement: aSelector
]

{ #category : #'*Deprecated12' }
ClassOrganization >> categoryOfElement: aSelector ifAbsent: aBlock [

	self
		deprecated: 'Use #protocolNameOfElement:ifAbsent: instead.'
		transformWith: '`@rcv categoryOfElement: `@arg1 ifAbsent: `@arg2' -> '`@rcv protocolNameOfElement: `@arg1 ifAbsent: `@arg2'.
	^ self protocolNameOfElement: aSelector ifAbsent: aBlock
]

{ #category : #'*Deprecated12' }
ClassOrganization >> listAtCategoryNamed: aName [

	self deprecated: 'Use #methodsInProtocolNamed: instead.' transformWith: '`@rcv listAtCategoryNamed: `@arg' -> '`@rcv methodsInProtocolNamed: `@arg'.
	^ self methodsInProtocolNamed: aName
]

{ #category : #'*Deprecated12' }
ClassOrganization >> notifyOfAddedCategory: protocolName [

	self deprecated: 'Use #notifyOfAddedProtocol: instead.' transformWith: '`@rcv notifyOfAddedCategory: `@arg' -> '`@rcv notifyOfAddedProtocol: `@arg'.

	self notifyOfAddedProtocol: protocolName
]
