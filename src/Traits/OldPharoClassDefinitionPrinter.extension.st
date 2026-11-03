Extension { #name : 'OldPharoClassDefinitionPrinter' }

{ #category : '*Traits' }
OldPharoClassDefinitionPrinter >> basicTraitDefinitionString [
	"Pay attention this definition is fragile in presence of complex slots. Use the public API i.e., classDefinitionString"

	self forClass instanceSide name == #Trait ifTrue: [ ^ self classDefinitionString ].

	^ String streamContents: [ :s |
			  s
				  nextPutAll: 'Trait named: ';
				  nextPutAll: self forClass name printString.

			  self applyCustomizersOn: s.
			  self instanceVariablesOn: s.
			  self packageOn: s ]
]

{ #category : '*Traits' }
OldPharoClassDefinitionPrinter >> basicTraitedMetaclassDefinitionString [

	^ String streamContents: [ :strm |
			  strm print: self forClass.

			  self applyCustomizersOn: strm.
			  self instanceVariablesOn: strm ]
]

{ #category : '*Traits' }
OldPharoClassDefinitionPrinter >> traitDefinitionString [

	^ self forClass needsSlotClassDefinition
		  ifTrue: [ (ClassDefinitionPrinter fluid for: self forClass) traitDefinitionString ]
		  ifFalse: [ self basicTraitDefinitionString ]
]

{ #category : '*Traits' }
OldPharoClassDefinitionPrinter >> traitedMetaclassDefinitionString [

	^ self forClass needsSlotClassDefinition
		  ifTrue: [ (ClassDefinitionPrinter fluid for: self forClass) traitedMetaclassDefinitionString ]
		  ifFalse: [ self basicTraitedMetaclassDefinitionString ]
]
