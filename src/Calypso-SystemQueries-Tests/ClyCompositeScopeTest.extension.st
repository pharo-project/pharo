Extension { #name : 'ClyCompositeScopeTest' }

{ #category : '*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToInheritedScope [

	| convertedScope |
	self scope: (ClyCompositeScope on: {
				 (ClyInstanceSideScope of: Object in: self environment).
				 (ClyClassSideScope of: Array in: self environment) }).

	convertedScope := self scope asInheritedScope.
	self assert: convertedScope class equals: ClyCompositeScope.
	self assert: (convertedScope subscopes first representsScope: ClySuperclassScope).
	self assert: (convertedScope subscopes last representsScope: ClySuperclassScope).
	self assert: (convertedScope subscopes collect: #localScopeClass as: Set) equals: {
			ClyInstanceSideScope.
			ClyClassSideScope } asSet.
	self assert: convertedScope environment equals: self environment.
	self assert: convertedScope name equals: ClyClassScope inheritedScopeName
]

{ #category : '*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToInheritingScope [

	| convertedScope |
	self scope: (ClyCompositeScope on: {
				 (ClyInstanceSideScope of: Object in: self environment).
				 (ClyClassSideScope of: Array in: self environment) }).

	convertedScope := self scope asInheritingScope.

	self assert: convertedScope class equals: ClyCompositeScope.
	self assert: (convertedScope subscopes first representsScope: ClySubclassScope).
	self assert: (convertedScope subscopes last representsScope: ClySubclassScope).
	self assert: (convertedScope subscopes collect: #localScopeClass as: Set) equals: {
			ClyInstanceSideScope.
			ClyClassSideScope } asSet.
	self assert: convertedScope environment equals: self environment.
	self assert: convertedScope name equals: ClyClassScope inheritingScopeName
]

{ #category : '*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToInterestingClassScope [

	| convertedScope |
	self scope: (ClyCompositeScope on: {
				 (ClySuperclassScope of: String in: self environment localScope: ClyInstanceSideScope).
				 (ClySubclassScope of: Array in: self environment localScope: ClyClassSideScope) }).

	convertedScope := self scope asInterestingClassScope.

	self assert: convertedScope class equals: ClyCompositeScope.
	self assert: convertedScope subscopes asSet equals: {
			(ClyInterestingSuperclassScope of: String localScope: ClyInstanceSideScope).
			(ClySubclassScope of: Array localScope: ClyClassSideScope) } asSet.
	self assert: convertedScope environment equals: self environment
]

{ #category : '*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToInterestingClassScopeShouldKeepScopeName [

	| convertedScope |
	self scope: (ClyCompositeScope on: {
				 (ClySuperclassScope of: String).
				 (ClySubclassScope of: Array) }).
	self scope name: 'test scope'.

	convertedScope := self scope asInterestingClassScope.

	self assert: convertedScope name equals: 'test scope'
]

{ #category : '*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToNewMetaLevel [

	| convertedScope |
	self scope: (ClyCompositeScope on: {
				 (ClyClassScope of: Object in: self environment).
				 (ClySubclassScope of: Array in: self environment) }).

	convertedScope := self scope withMetaLevel: ClyInstanceSideScope.

	self assert: convertedScope class equals: ClyCompositeScope.
	self assert: convertedScope subscopes first equals: (ClyInstanceSideScope of: Object).
	self assert: convertedScope subscopes last class equals: ClySubclassScope.
	self assert: convertedScope subscopes last localScopeClass equals: ClyInstanceSideScope.
	self assert: convertedScope environment equals: self environment.
	self assert: (convertedScope subscopes collect: #environment as: Set) equals: { self environment } asSet
]

{ #category : '*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToNewMetaLevelShouldKeepScopeName [

	| convertedScope |
	self scope: (ClyCompositeScope on: {
				 (ClyClassScope of: Object in: self environment).
				 (ClySubclassScope of: Array in: self environment) }).
	self scope name: 'test scope'.

	convertedScope := self scope withMetaLevel: ClyInstanceSideScope.

	self assert: convertedScope name equals: 'test scope'
]
