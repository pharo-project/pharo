Extension { #name : 'OldPharoClassDefinitionPrinter' }

{ #category : '*Traits' }
OldPharoClassDefinitionPrinter >> basicTraitDefinitionString [
	"Pay attention this definition is fragile in presence of complex slots. Use the public API i.e., classDefinitionString"

	forClass instanceSide name == #Trait ifTrue: [ ^ self classDefinitionString ].

	^ String streamContents: [ :s |
			  s
				  nextPutAll: 'Trait named: ';
				  nextPutAll: forClass name printString.

			  self applyCustomizersOn: s.
			  self instanceVariablesOn: s.
			  self packageOn: s ]
]

{ #category : '*Traits' }
OldPharoClassDefinitionPrinter >> basicTraitedMetaclassDefinitionString [

	^ String streamContents: [ :strm |
			  strm print: forClass.

			  self applyCustomizersOn: strm.
			  self instanceVariablesOn: strm ]
]

{ #category : '*Traits' }
OldPharoClassDefinitionPrinter >> traitDefinitionString [

	^ forClass needsSlotClassDefinition
		ifTrue: [ (ClassDefinitionPrinter fluid for: forClass) traitDefinitionString ]
		ifFalse: [ self basicTraitDefinitionString ]
]

{ #category : '*Traits' }
OldPharoClassDefinitionPrinter >> traitedMetaclassDefinitionString [

	^ forClass needsSlotClassDefinition
		ifTrue: [ (ClassDefinitionPrinter fluid for: forClass) traitedMetaclassDefinitionString ]
		ifFalse: [ self basicTraitedMetaclassDefinitionString ]
]
