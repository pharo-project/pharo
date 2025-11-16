Extension { #name : 'MCScriptDefinition' }

{ #category : '*Monticello' }
MCScriptDefinition >> accept: aVisitor [
	aVisitor visitScriptDefinition: self
]
