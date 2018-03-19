Extension { #name : #ClassDescription }

{ #category : #'*TraitsV2' }
ClassDescription >> addToComposition: aTrait [
	self setTraitComposition: self traitComposition + aTrait
]

{ #category : #'*TraitsV2' }
ClassDescription >> asTraitComposition [

	^ TaCompositionElement for: self
]

{ #category : #'*TraitsV2' }
ClassDescription >> baseComposition [
	^ TaEmptyComposition new
]

{ #category : #'*TraitsV2' }
ClassDescription >> traitComposition [
	^ TaEmptyComposition new
]
