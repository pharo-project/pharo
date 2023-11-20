Extension { #name : 'ClassDescription' }

{ #category : '*Traits' }
ClassDescription >> addToComposition: aTrait [
	self setTraitComposition: self traitComposition + aTrait
]

{ #category : '*Traits' }
ClassDescription >> asTraitComposition [

	^ TaCompositionElement for: self
]

{ #category : '*Traits' }
ClassDescription >> baseComposition [
	^ TaEmptyComposition new
]

{ #category : '*Traits' }
ClassDescription >> selectorsWithExplicitOrigin [
	^self traitComposition selectors , self localSelectors
]

{ #category : '*Traits' }
ClassDescription >> traitComposition [
	^ TaEmptyComposition new
]
