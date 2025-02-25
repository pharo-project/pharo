Extension { #name : 'ClyLocalClassScopeTest' }

{ #category : '*Calypso-SystemPlugins-Traits-Queries-Tests' }
ClyLocalClassScopeTest >> testConvertingToInheritedScopeShouldAddInheritedTraits [
	| convertedScope |
	scope := self createSampleScope.

	convertedScope := scope asInheritedScope.

	self assert: (convertedScope representsScope: ClyInheritedTraitScope)
]

{ #category : '*Calypso-SystemPlugins-Traits-Queries-Tests' }
ClyLocalClassScopeTest >> testConvertingToInheritingScopeShouldAddTraitUsers [
	| convertedScope |
	scope := self createSampleScope.

	convertedScope := scope asInheritingScope.

	self assert: (convertedScope representsScope: ClyTraitUserScope)
]
