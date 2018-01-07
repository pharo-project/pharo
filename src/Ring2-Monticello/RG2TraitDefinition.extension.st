Extension { #name : #RG2TraitDefinition }

{ #category : #'*Ring2-Monticello' }
RG2TraitDefinition >> asMCDefinition [

	^ MCTraitDefinition 
		name: self name
		traitComposition: self traitCompositionString 
		category: self category 
		comment: self comment content
		commentStamp: ''
]
