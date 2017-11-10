Extension { #name : #Trait }

{ #category : #'*Monticello' }
Trait >> asClassDefinition [
	^ MCTraitDefinition
		name: self name
		traitComposition: self traitCompositionString
		category: self category 
		comment: self organization classComment asString
		commentStamp: self organization commentStamp.
]

{ #category : #'*Monticello' }
Trait >> classDefinitions [
	| definitions |
	definitions := OrderedCollection with: self asClassDefinition.
	(self hasClassSide
		and: [self classTrait hasTraitComposition])
					ifTrue: [definitions add: self classTrait asMCDefinition].
	^definitions asArray
]
