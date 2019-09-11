Class {
	#name : #CDClassWithTraitExclusionParserTest,
	#superclass : #CDTraitCompositionClassParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDClassWithTraitExclusionParserTest >> testTraitDefinitionNodeIsTraitAliasNode [

	self assert: self traitCompositionNode isTraitExclusionNode
]

{ #category : #helpers }
CDClassWithTraitExclusionParserTest >> traitDefinition [

	^ 'MyTrait - { #selector1 }'
]
