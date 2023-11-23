Class {
	#name : 'CDNormalMetaclassParserTest',
	#superclass : 'CDMetaclassParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDNormalMetaclassParserTest >> classDefinitionString [

	^ '{superclassName} class << {className} class
		traits: {classTraitName} classTrait;
		slots: {slots}' format: {
			  ('className' -> self className).
			  ('superclassName' -> self superclassName).
			  ('slots' -> self slotsArray).
			  ('classTraitName' -> self traitDefinition) } asDictionary
]

{ #category : 'helpers' }
CDNormalMetaclassParserTest >> testTraitDefinitionNodeIsTraitNode [

	self assert: self traitCompositionNode isTraitNode
]

{ #category : 'helpers' }
CDNormalMetaclassParserTest >> traitCompositionNode [

	^ self nodeFor: self traitDefinition
]

{ #category : 'helpers' }
CDNormalMetaclassParserTest >> traitDefinition [

	^ 'TEmpty'
]
