Class {
	#name : #CDClassWithTraitParserTest,
	#superclass : #CDTraitCompositionClassParserTest,
	#category : #'ClassParser-Test'
}

{ #category : #helpers }
CDClassWithTraitParserTest >> testTraitDefinitionNodeIsTraitNode [

	self assert: self traitCompositionNode isTraitNode
]

{ #category : #helpers }
CDClassWithTraitParserTest >> traitDefinition [

	^ 'MyTrait'
]
