"
I am a refactoring change for the class side of a class addition.
"
Class {
	#name : #RBAddMetaclassChange,
	#superclass : #RBRefactoryDefinitionChange,
	#instVars : [
		'classInstanceVariableNames'
	],
	#category : #'Refactoring-Changes-Changes-Classes'
}

{ #category : #private }
RBAddMetaclassChange class >> definitionPatterns [

	^ #(
		'`className class instanceVariableNames: `#instanceVariableNames'
		'`className class uses: `@traitComposition instanceVariableNames: `#instanceVariableNames'
		'`className class uses: `@traitComposition slots: `#instanceVariableNames'
		'`className class slots: `#instanceVariableNames'
		'`className class'
		)
]

{ #category : #converting }
RBAddMetaclassChange >> asUndoOperation [

	^ self class definition: self changeClass class oldDefinition
]

{ #category : #accessing }
RBAddMetaclassChange >> classInstanceVariableNames [

	^ classInstanceVariableNames
]

{ #category : #private }
RBAddMetaclassChange >> definitionClass [

	^ self changeClass
]

{ #category : #initialization }
RBAddMetaclassChange >> fillOutDefinition: aDictionary [

	className := (aDictionary at: '`className') asSymbol.
	classInstanceVariableNames := self namesIn:
		                              (aDictionary
			                               at: '`#instanceVariableNames'
			                               ifAbsent: [ String new ])
]
