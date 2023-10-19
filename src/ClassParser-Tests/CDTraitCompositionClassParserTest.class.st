Class {
	#name : 'CDTraitCompositionClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'testing' }
CDTraitCompositionClassParserTest class >> isAbstract [
	^ self == CDTraitCompositionClassParserTest
]

{ #category : 'helpers' }
CDTraitCompositionClassParserTest >> classDefinitionString [

^ '{superclassName} << #{classname}
		traits: {traitDefinition};
		slots: {slots};
		sharedVariables: {sharedVariables};
		package: #MyPackage'
			format: {
				'classname' -> self className.
				'superclassName' -> self superclassName.
				'slots' -> self slotsArray.
				'sharedVariables' -> self sharedVariablesArray.
				'traitDefinition' -> self traitDefinition } asDictionary
]

{ #category : 'tests' }
CDTraitCompositionClassParserTest >> testBestNodeForTraitCompositionSelectionShouldBeTraitComposition [

	| selectedNode start selection |
	start := self traitDefinitionIndex.
	selection := start to: start.

	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode isTraitComposition
]

{ #category : 'helpers' }
CDTraitCompositionClassParserTest >> traitCompositionNode [

	^ self nodeFor: self traitDefinition
]

{ #category : 'helpers' }
CDTraitCompositionClassParserTest >> traitDefinition [

	self subclassResponsibility
]

{ #category : 'helpers' }
CDTraitCompositionClassParserTest >> traitDefinitionIndex [

	^ self classDefinitionString findString: self traitDefinition
]
