Extension { #name : #RGBehaviorDefinition }

{ #category : #'*Epicea-Ring' }
RGBehaviorDefinition >> traitComposition [
	
	^ (self class compiler evaluate: self traitCompositionSource) asTraitComposition
]
