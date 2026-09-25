Extension { #name : 'ClyLocalClassScopeTest' }

{ #category : '*Calypso-SystemPlugins-Traits-Queries-Tests' }
ClyLocalClassScopeTest >> testConvertingToInheritedScopeShouldAddInheritedTraits [

	| convertedScope |
	self scope: self createSampleScope.

	convertedScope := self scope asInheritedScope.

	self assert: (convertedScope representsScope: ClyInheritedTraitScope)
]

{ #category : '*Calypso-SystemPlugins-Traits-Queries-Tests' }
ClyLocalClassScopeTest >> testConvertingToInheritingScopeShouldAddTraitUsers [

	| convertedScope |
	self scope: self createSampleScope.

	convertedScope := self scope asInheritingScope.

	self assert: (convertedScope representsScope: ClyTraitUserScope)
]
