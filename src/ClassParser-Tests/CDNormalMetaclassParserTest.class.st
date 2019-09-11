Class {
	#name : #CDNormalMetaclassParserTest,
	#superclass : #CDMetaclassParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDNormalMetaclassParserTest >> classDefinitionString [
	
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
CDNormalMetaclassParserTest >> classTraitName [
	^ 'TApplyingOnClassSide'
]

{ #category : #helpers }
CDNormalMetaclassParserTest >> testTraitDefinitionNodeIsTraitNode [

	self assert: self traitCompositionNode isTraitNode
]

{ #category : #helpers }
CDNormalMetaclassParserTest >> traitCompositionNode [
	
	^ self nodeFor: self traitDefinition
]

{ #category : #helpers }
CDNormalMetaclassParserTest >> traitDefinition [

	^ self classTraitName
]
