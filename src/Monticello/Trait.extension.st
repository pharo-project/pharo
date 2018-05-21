Extension { #name : #Trait }

{ #category : #'*Monticello' }
Trait >> asClassDefinition [
	self usesSpecialVariables ifFalse: [  
		^ MCTraitDefinition
			name: self name
			traitComposition: self traitCompositionString
			category: self category 
			instVarNames: self instVarNames
			comment: self organization classComment asString
			commentStamp: self organization commentStamp ].
		
	^ MCTraitDefinition
			name: self name
			traitComposition: self traitCompositionString
			category: self category 
			instVarNames: (self localSlots collect: #definitionString)
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
