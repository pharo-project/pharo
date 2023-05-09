Extension { #name : #ClassOrganization }

{ #category : #'*Deprecated12' }
ClassOrganization >> addCategory: aString [

	self deprecated: 'Use #addProtocol: instead.' transformWith: '`@rcv addCategory: `@arg' -> '`@rcv addProtocol: `@arg'.
	self addProtocol: aString
]

{ #category : #'*Deprecated12' }
ClassOrganization >> allCategories [

	self deprecated: 'Use #protocolNames instead.' transformWith: '`@rcv allCategories' -> '`@rcv protocolNames'.
	^ self protocolNames
]

{ #category : #'*Deprecated12' }
ClassOrganization >> categories [

	self deprecated: 'Use #protocolNames instead.' transformWith: '`@rcv categories' -> '`@rcv protocolNames'.
	^ self protocolNames
]

{ #category : #'*Deprecated12' }
ClassOrganization >> categoryOfElement: aSelector [

	self deprecated: 'Use #protocolNameOfElement: instead.' transformWith: '`@rcv categoryOfElement: `@arg' -> '`@rcv protocolNameOfElement: `@arg'.
	^ self protocolNameOfElement: aSelector
]

{ #category : #'*Deprecated12' }
ClassOrganization >> categoryOfElement: aSelector ifAbsent: aBlock [

	self
		deprecated: 'Use #protocolNameOfElement: instead.'
		transformWith: '`@rcv categoryOfElement: `@arg1 ifAbsent: `@arg2' -> '(`@rcv protocolNameOfElement: `@arg1) ifNil: `@arg2'.
	^ (self protocolNameOfElement: aSelector) ifNil: aBlock
]

{ #category : #'*Deprecated12' }
ClassOrganization >> classComment [

	self
		deprecated: 'The organization is not responsible for the class comment anymore. Ask the comment to the class directly.'
		on: '18 April 2023'
		in: 'Pharo-12.0.0+build.183.sha.cf9b49f2a83f05bb9f02ac5831035d1bda76a14a (64 Bit)'.

	^ self comment
]

{ #category : #'*Deprecated12' }
ClassOrganization >> classComment: aString [

	self
		deprecated: 'The organization is not responsible for the class comment anymore. Ask the comment to the class directly.'
		on: '18 April 2023'
		in: 'Pharo-12.0.0+build.183.sha.cf9b49f2a83f05bb9f02ac5831035d1bda76a14a (64 Bit)'.
	self comment: aString
]

{ #category : #'*Deprecated12' }
ClassOrganization >> comment [

	self
		deprecated: 'The organization is not responsible for the class comment anymore. Ask the comment to the class directly.'
		on: '18 April 2023'
		in: 'Pharo-12.0.0+build.183.sha.cf9b49f2a83f05bb9f02ac5831035d1bda76a14a (64 Bit)'.
	^ organizedClass comment
]

{ #category : #'*Deprecated12' }
ClassOrganization >> comment: aString [

	self
		deprecated: 'The organization is not responsible for the class comment anymore. Ask the comment to the class directly.'
		on: '18 April 2023'
		in: 'Pharo-12.0.0+build.183.sha.cf9b49f2a83f05bb9f02ac5831035d1bda76a14a (64 Bit)'.
	organizedClass comment: aString
]

{ #category : #'*Deprecated12' }
ClassOrganization >> commentSourcePointer [

	self
		deprecated: 'The organization is not responsible for the class comment anymore. Ask the comment to the class directly.'
		on: '18 April 2023'
		in: 'Pharo-12.0.0+build.183.sha.cf9b49f2a83f05bb9f02ac5831035d1bda76a14a (64 Bit)'.
	^ organizedClass commentSourcePointer
]

{ #category : #'*Deprecated12' }
ClassOrganization >> commentSourcePointer: anObject [

	self
		deprecated: 'The organization is not responsible for the class comment anymore. Ask the comment to the class directly.'
		on: '18 April 2023'
		in: 'Pharo-12.0.0+build.183.sha.cf9b49f2a83f05bb9f02ac5831035d1bda76a14a (64 Bit)'.
	organizedClass commentSourcePointer: anObject
]

{ #category : #'*Deprecated12' }
ClassOrganization >> commentStamp [

	self
		deprecated: 'The organization is not responsible for the class comment anymore. Ask the comment to the class directly.'
		on: '18 April 2023'
		in: 'Pharo-12.0.0+build.183.sha.cf9b49f2a83f05bb9f02ac5831035d1bda76a14a (64 Bit)'.
	^ organizedClass commentStamp
]

{ #category : #'*Deprecated12' }
ClassOrganization >> hasComment [

	self
		deprecated: 'The organization is not responsible for the class comment anymore. Ask the comment to the class directly.'
		on: '18 April 2023'
		in: 'Pharo-12.0.0+build.183.sha.cf9b49f2a83f05bb9f02ac5831035d1bda76a14a (64 Bit)'.
	^ organizedClass hasComment
]

{ #category : #'*Deprecated12' }
ClassOrganization >> listAtCategoryNamed: aName [

	self deprecated: 'Use #methodsInProtocolNamed: instead.' transformWith: '`@rcv listAtCategoryNamed: `@arg' -> '`@rcv methodSelectorsInProtocol: `@arg'.
	^ self methodSelectorsInProtocol: aName
]

{ #category : #'*Deprecated12' }
ClassOrganization >> removeCategory: protocolName [

	self deprecated: 'Use #removeProtocolIfEmpty: instead.' transformWith: '`@rcv removeCategory: `@arg' -> '`@rcv removeProtocolIfEmpty: `@arg'.
	self removeProtocolIfEmpty: protocolName
]

{ #category : #'*Deprecated12' }
ClassOrganization >> removeEmptyCategories [

	self deprecated: 'Use #removeEmptyProtocols instead.' transformWith: '`@rcv removeEmptyCategories' -> '`@rcv removeEmptyProtocols'.
	self removeEmptyProtocols
]

{ #category : #'*Deprecated12' }
ClassOrganization >> removeProtocol: aProtocol [

	self
		deprecated: 'Use #removeProtocolIfEmpty: since I was also removing only empty protocols.'
		transformWith: '`@rcv removeProtocol: `@arg' -> '`@rcv removeProtocolIfEmpty: `@arg'.
	^ self removeProtocolIfEmpty: aProtocol
]

{ #category : #'*Deprecated12' }
ClassOrganization >> removeProtocolNamed: protocolName [

	self deprecated: 'Use #removeProtocolIfEmpty: instead' transformWith: '`@rcv removeProtocolNamed: `@arg' -> '`@rcv removeProtocolIfEmpty: `@arg'.
	^ self removeProtocolIfEmpty: protocolName
]

{ #category : #'*Deprecated12' }
ClassOrganization >> renameCategory: oldName toBe: newName [

	self
		deprecated: 'Use #renameProtocol:as: instead.'
		transformWith: '`@rcv renameCategory: `@arg1 toBe: `@arg2' -> '`@rcv renameProtocol: `@arg1 as: `@arg2'.
	self renameProtocol: oldName as: newName
]

{ #category : #'*Deprecated12' }
ClassOrganization >> silentlyRenameCategory: oldName toBe: newName [

	self
		deprecated: 'Use #renameProtocol:as: instead. Since Pharo12 it is not possible to not announce a protocol rename anymore because we should always announce those changes.'
		transformWith: '`@rcv silentlyRenameCategory: `@arg1 toBe: `@arg2' -> '`@rcv renameProtocol: `@arg1 as: `@arg2'.
	self renameProtocol: oldName as: newName
]

{ #category : #'*Deprecated12' }
ClassOrganization >> subject [

	self deprecated: 'Use #organizedClass instead.' transformWith: '`@rcv subject' -> '`@rcv organizedClass'.
	^ self organizedClass
]
