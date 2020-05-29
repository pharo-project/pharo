Extension { #name : #Trait }

{ #category : #'*Monticello' }
Trait >> asClassDefinition [

	| classTraitCompositionToUse |

	classTraitCompositionToUse := self class traitCompositionString.

	self needsSlotClassDefinition ifFalse: [  
		^ MCTraitDefinition
			name: self name
			traitComposition: self traitCompositionString
			category: self category 
			instVarNames: (self localSlots collect: #name)
			classInstVarNames: (self class localSlots collect: #name)
			classTraitComposition: classTraitCompositionToUse
			comment: self organization classComment asString
			commentStamp: self organization commentStamp ].
		
	^ MCTraitDefinition
			name: self name
			traitComposition: self traitCompositionString
			category: self category 
			instVarNames: (self localSlots collect: #definitionString)
			classInstVarNames: (self classSide localSlots collect: #definitionString)
			classTraitComposition: classTraitCompositionToUse
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
