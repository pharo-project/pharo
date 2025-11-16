Extension { #name : 'MCMethodDefinition' }

{ #category : '*Monticello' }
MCMethodDefinition >> accept: aVisitor [
	^ aVisitor visitMethodDefinition: self
]
