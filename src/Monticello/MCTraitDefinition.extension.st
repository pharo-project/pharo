Extension { #name : 'MCTraitDefinition' }

{ #category : '*Monticello' }
MCTraitDefinition >> accept: aVisitor [
	^ aVisitor visitTraitDefinition: self

]
