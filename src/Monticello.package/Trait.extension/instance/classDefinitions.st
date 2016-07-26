*Monticello
classDefinitions
	| definitions |
	definitions := OrderedCollection with: self asClassDefinition.
	(self hasClassSide
		and: [self classTrait hasTraitComposition])
					ifTrue: [definitions add: self classTrait asMCDefinition].
	^definitions asArray