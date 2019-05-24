"
I am a refactoring change for a class trait  addition, the meta side of a trait definition.
"
Class {
	#name : #RBAddClassTraitChange,
	#superclass : #RBRefactoryDefinitionChange,
	#category : #'Refactoring-Changes'
}

{ #category : #private }
RBAddClassTraitChange class >> definitionPatterns [
	^ #('`traitName classTrait uses: `@traitComposition')
]

{ #category : #converting }
RBAddClassTraitChange >> asUndoOperation [
	^ self class definition: self changeClass classTrait oldDefinition
]

{ #category : #private }
RBAddClassTraitChange >> definitionClass [
	^ self changeClass
]

{ #category : #initialization }
RBAddClassTraitChange >> fillOutDefinition: aDictionary [
	className := (aDictionary at: '`traitName') asSymbol
]
