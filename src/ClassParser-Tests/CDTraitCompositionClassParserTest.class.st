Class {
	#name : #CDTraitCompositionClassParserTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #testing }
CDTraitCompositionClassParserTest class >> isAbstract [
	^ self == CDTraitCompositionClassParserTest
]

{ #category : #helpers }
CDTraitCompositionClassParserTest >> classDefinitionString [

^ '{superclassName} subclass: #{classname}
		uses: {traitDefinition}
		instanceVariableNames: ''{instvar1} {instvar2}''
		classVariableNames: ''{classvar1} {classvar2}''
		package: #MyPackage'
			format: {
				'classname' -> self className.
				'superclassName' -> self superclassName.
				'instvar1' -> self firstInstanceVariableName.
				'instvar2' -> self secondInstanceVariableName.
				'classvar1' -> self firstClassVariableName.
				'classvar2' -> self secondClassVariableName.
				'traitDefinition' -> self traitDefinition } asDictionary
]

{ #category : #tests }
CDTraitCompositionClassParserTest >> testBestNodeForTraitCompositionSelectionShouldBeTraitComposition [

	| selectedNode start selection |	
	start := self traitDefinitionIndex.
	selection := start to: start.
	
	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode isTraitComposition
]

{ #category : #helpers }
CDTraitCompositionClassParserTest >> traitCompositionNode [
	
	^ self nodeFor: self traitDefinition
]

{ #category : #helpers }
CDTraitCompositionClassParserTest >> traitDefinition [

	self subclassResponsibility
]

{ #category : #helpers }
CDTraitCompositionClassParserTest >> traitDefinitionIndex [

	^ self classDefinitionString findString: self traitDefinition
]
