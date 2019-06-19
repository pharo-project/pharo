Extension { #name : #ClyMessageImplementorsQueryTest }

{ #category : #'*Calypso-SystemPlugins-Traits-Queries-Tests' }
ClyMessageImplementorsQueryTest >> testFromClassAndInheritedTrait [
	
	query := ClyMessageImplementorsQuery of: #methodFromRoot1.
	self queryFromScope: ClyBothMetaLevelClassScope ofAll: { ClyClassWithTraits. ClyTraitRoot1 }.
	
	self assert: resultItems size equals: 1.
	self assert: resultItems first == (ClyTraitRoot1 >> #methodFromRoot1)
]
