Class {
	#name : 'CDMetaclassWithTraitParserTest',
	#superclass : 'CDMetaclassParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDMetaclassWithTraitParserTest >> classDefinitionString [

	^ '{superclassName} class << {className} class
		traits: {classTraitName} classTrait;
		slots: {slots}'
			format: {
				'className' -> self className.
				'superclassName' -> self superclassName.
				'slots' -> self slotsArray.
				'classTraitName' -> self classTraitName
			 } asDictionary
]

{ #category : 'helpers' }
CDMetaclassWithTraitParserTest >> classTraitName [
	^ 'TApplyingOnClassSide'
]

{ #category : 'helpers' }
CDMetaclassWithTraitParserTest >> testTraitDefinitionNodeIsTraitNode [

	self assert: self traitCompositionNode isTraitNode
]

{ #category : 'helpers' }
CDMetaclassWithTraitParserTest >> traitCompositionNode [

	^ self nodeFor: self traitDefinition
]

{ #category : 'helpers' }
CDMetaclassWithTraitParserTest >> traitDefinition [

	^ self classTraitName
]
