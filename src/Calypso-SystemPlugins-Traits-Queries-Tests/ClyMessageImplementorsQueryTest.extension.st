Extension { #name : 'ClyMessageImplementorsQueryTest' }

{ #category : '*Calypso-SystemPlugins-Traits-Queries-Tests' }
ClyMessageImplementorsQueryTest >> testFromClassAndInheritedTrait [
	self query: (ClyMessageImplementorsQuery of: #methodFromRoot1).
	self queryFromScope: ClyBothMetaLevelClassScope ofAll: {ClyClassWithTraits . ClyTraitRoot1}.

	self assert: self resultItems size equals: 1.
	self assert: self resultItems first identicalTo: ClyTraitRoot1 >> #methodFromRoot1
]
