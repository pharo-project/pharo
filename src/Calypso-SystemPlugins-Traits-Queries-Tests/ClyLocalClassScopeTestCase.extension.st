Extension { #name : #ClyLocalClassScopeTestCase }

{ #category : #'*Calypso-SystemPlugins-Traits-Queries-Tests' }
ClyLocalClassScopeTestCase >> testConvertingToInheritedScopeShouldAddInheritedTraits [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope asInheritedScope.
	
	self assert: (convertedScope representsScope: ClyInheritedTraitScope)
]

{ #category : #'*Calypso-SystemPlugins-Traits-Queries-Tests' }
ClyLocalClassScopeTestCase >> testConvertingToInheritingScopeShouldAddTraitUsers [
	| convertedScope |
	scope := self createSampleScope.
	
	convertedScope := scope asInheritingScope.
	
	self assert: (convertedScope representsScope: ClyTraitUserScope)
]
