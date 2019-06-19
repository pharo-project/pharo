Extension { #name : #ClyCompositeScopeTest }

{ #category : #'*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToInheritedScope [
	| convertedScope |
	scope := ClyCompositeScope on: {
		ClyInstanceSideScope of: Object in: environment.
		ClyClassSideScope of: Array in: environment}.
		
	convertedScope := scope asInheritedScope.
	
	self assert: convertedScope class equals: ClyCompositeScope.
	self assert: (convertedScope subscopes first representsScope: ClySuperclassScope).
	self assert: (convertedScope subscopes last representsScope: ClySuperclassScope).
	self
		assert: (convertedScope subscopes collect: #localScopeClass as: Set)
		equals: {ClyInstanceSideScope. ClyClassSideScope} asSet.
	self assert: convertedScope environment equals: environment.
	self assert: convertedScope name equals: ClyClassScope inheritedScopeName
]

{ #category : #'*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToInheritingScope [
	| convertedScope |
	scope := ClyCompositeScope on: {
		ClyInstanceSideScope of: Object in: environment.
		ClyClassSideScope of: Array in: environment}.
		
	convertedScope := scope asInheritingScope.
	
	self assert: convertedScope class equals: ClyCompositeScope.
	self assert: (convertedScope subscopes first representsScope: ClySubclassScope).
	self assert: (convertedScope subscopes last representsScope: ClySubclassScope).
	self
		assert: (convertedScope subscopes collect: #localScopeClass as: Set)
		equals: {ClyInstanceSideScope. ClyClassSideScope} asSet.
	self assert: convertedScope environment equals: environment.
	self assert: convertedScope name equals: ClyClassScope inheritingScopeName
]

{ #category : #'*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToInterestingClassScope [
	| convertedScope |
	scope := ClyCompositeScope on: { 
		ClySuperclassScope of: String in: environment localScope: ClyInstanceSideScope.
		ClySubclassScope of: Array in: environment localScope: ClyClassSideScope }.
	
	convertedScope := scope asInterestingClassScope.
	
	self assert: convertedScope class equals: ClyCompositeScope.
	self 
		assert: convertedScope subscopes asSet 
		equals: {
			ClyInterestingSuperclassScope of: String localScope: ClyInstanceSideScope.
			ClySubclassScope of: Array localScope: ClyClassSideScope} asSet.
	self assert: convertedScope environment equals: environment.
]

{ #category : #'*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToInterestingClassScopeShouldKeepScopeName [
	| convertedScope |
	scope := ClyCompositeScope on: {
		ClySuperclassScope of: String. ClySubclassScope of: Array}.
	scope name: 'test scope'.
	
	convertedScope := scope asInterestingClassScope.
	
	self assert: convertedScope name equals: 'test scope'
]

{ #category : #'*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToNewMetaLevel [
	| convertedScope |
	scope := ClyCompositeScope on: {
		ClyClassScope of: Object in: environment.
		ClySubclassScope of: Array in: environment}.
		
	convertedScope := scope withMetaLevel: ClyInstanceSideScope.
	
	self assert: convertedScope class equals: ClyCompositeScope.
	self
		assert: convertedScope subscopes first
		equals: (ClyInstanceSideScope of: Object).
	self
		assert: convertedScope subscopes last class
		equals: ClySubclassScope.
	self
		assert: convertedScope subscopes last localScopeClass
		equals: ClyInstanceSideScope.
	self assert: convertedScope environment equals: environment.
	self
		assert: (convertedScope subscopes collect: #environment as: Set)
		equals: {environment} asSet
]

{ #category : #'*Calypso-SystemQueries-Tests' }
ClyCompositeScopeTest >> testConvertingToNewMetaLevelShouldKeepScopeName [
	| convertedScope |
	scope := ClyCompositeScope on: {
		ClyClassScope of: Object in: environment.
		ClySubclassScope of: Array in: environment}.
	scope name: 'test scope'.
		
	convertedScope := scope withMetaLevel: ClyInstanceSideScope.
	
	self assert: convertedScope name equals: 'test scope'
]
