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

{ #category : #'*Deprecated12' }
ClassOrganization >> notifyOfChangedCategoriesFrom: oldProtocolName to: newProtocolName [

	self
		deprecated: 'Use #notifyOfChangedProtocolNamesFrom:to: instead.'
		transformWith: '`@rcv notifyOfChangedCategoriesFrom: `@arg1 to: `@arg2' -> '`@rcv notifyOfChangedProtocolNamesFrom: `@arg1 to: `@arg2'.
	self notifyOfChangedProtocolNamesFrom: oldProtocolName to: newProtocolName
]

{ #category : #'*Deprecated12' }
ClassOrganization >> notifyOfChangedCategoryFrom: oldNameOrNil to: newNameOrNil [

	self
		deprecated: 'Use #notifyOfChangedProtocolNameFrom: instead.'
		transformWith: '`@rcv notifyOfChangedCategoryFrom: `@arg1 to: `@arg2' -> '`@rcv notifyOfChangedProtocolNameFrom: `@arg1 to: `@arg2'.
	self notifyOfChangedProtocolNameFrom: oldNameOrNil to: newNameOrNil
]

{ #category : #'*Deprecated12' }
ClassOrganization >> notifyOfRemovedCategory: protocolName [

	self
		deprecated: 'Use #notifyOfRemovedProtocolNamed: instead.'
		transformWith: '`@rcv notifyOfRemovedCategory: `@arg' -> '`@rcv notifyOfRemovedProtocolNamed: `@arg'.
	self notifyOfRemovedProtocolNamed: protocolName
]

{ #category : #'*Deprecated12' }
ClassOrganization >> removeCategory: protocolName [

	self deprecated: 'Use #removeProtocolNamed: instead.' transformWith: '`@rcv removeCategory: `@arg' -> '`@rcv removeProtocolNamed: `@arg'.
	self removeProtocolNamed: protocolName
]

{ #category : #'*Deprecated12' }
ClassOrganization >> removeEmptyCategories [

	self deprecated: 'Use #removeEmptyProtocols instead.' transformWith: '`@rcv removeEmptyCategories' -> '`@rcv removeEmptyProtocols'.
	self removeEmptyProtocols
]

{ #category : #'*Deprecated12' }
ClassOrganization >> renameCategory: oldName toBe: newName [

	self
		deprecated: 'Use #renameProtocolNamed:toBe: instead.'
		transformWith: '`@rcv renameCategory: `@arg1 toBe: `@arg2' -> '`@rcv renameProtocolNamed: `@arg1 toBe: `@arg2'.
	self renameProtocolNamed: oldName toBe: newName
]
