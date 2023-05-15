"
I am a refactoring change for a class trait  addition, the meta side of a trait definition.
"
Class {
	#name : #RBAddClassTraitChange,
	#superclass : #RBRefactoryDefinitionChange,
	#category : #'Refactoring-Changes-Changes-Classes'
}

{ #category : #private }
RBAddClassTraitChange class >> definitionPatterns [

	^ #(
		'`traitName classTrait uses: `@traitComposition' '`traitName classTrait'
		'`traitName classTrait uses: `@traitComposition instanceVariableNames: `@instVars'
		'`traitName classTrait instanceVariableNames: `@instVars'

		'`traitName classTrait uses: `@traitComposition slots: `@instVars'
		'`traitName classTrait slots: `@instVars'
	)

"I do not think that this correct because slots: expects an array an not a string as in instanceVariableNames: so I doubt that corresponding patterns are correct."
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
	"What a bad method name!!! This has nothing to do with it. It fills up a domain object out of the dictionary."
	"Pay attention that the variables used as key in the dictionary correspond to the pattern variables defined
	in the expressions defined in the definitionPatterns method!"
	className := (aDictionary at: '`traitName') asSymbol
]
