Class {
	#name : #CDMetaclassWithTraitParserTest,
	#superclass : #CDMetaclassParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDMetaclassWithTraitParserTest >> classDefinitionString [
	
	^ '{className} class 
		uses: {classTraitName} classTrait
		instanceVariableNames: ''{instvar1} {instvar2}'''
			format: { 
				'className' -> self className.	
				'instvar1' -> self firstInstanceVariableName.
				'instvar2' -> self secondInstanceVariableName.
				'classTraitName' -> self classTraitName
			 } asDictionary
			
	
]

{ #category : #helpers }
CDMetaclassWithTraitParserTest >> classTraitName [
	^ 'TApplyingOnClassSide'
]

{ #category : #helpers }
CDMetaclassWithTraitParserTest >> testTraitDefinitionNodeIsTraitNode [

	self assert: self traitCompositionNode isTraitNode
]

{ #category : #helpers }
CDMetaclassWithTraitParserTest >> traitCompositionNode [
	
	^ self nodeFor: self traitDefinition
]

{ #category : #helpers }
CDMetaclassWithTraitParserTest >> traitDefinition [

	^ self classTraitName
]
