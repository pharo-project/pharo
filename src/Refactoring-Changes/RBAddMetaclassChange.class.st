"
I am a refactoring change for the class side of a class addition.
"
Class {
	#name : 'RBAddMetaclassChange',
	#superclass : 'RBRefactoryDefinitionChange',
	#instVars : [
		'classInstanceVariableNames'
	],
	#category : 'Refactoring-Changes-Changes-Classes',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Classes'
}

{ #category : 'private' }
RBAddMetaclassChange class >> definitionPatterns [

	^ #(
		'`className class instanceVariableNames: `#instanceVariableNames'
		'`className class uses: `@traitComposition instanceVariableNames: `#instanceVariableNames'
		'`className class uses: `@traitComposition slots: `#instanceVariableNames'
		'`className class slots: `@slots'
		'`className class'
		)
]

{ #category : 'converting' }
RBAddMetaclassChange >> asUndoOperation [

	^ self class definition: self changeClass class oldDefinition
]

{ #category : 'accessing' }
RBAddMetaclassChange >> classInstanceVariableNames [

	^ classInstanceVariableNames
]

{ #category : 'private' }
RBAddMetaclassChange >> definitionClass [

	^ self changeClass
]

{ #category : 'initialization' }
RBAddMetaclassChange >> fillOutDefinition: aDictionary [
	"What a bad method name!!! This has nothing to do with it. It fills up a domain object out of the dictionary."
	"Pay attention that the variables used as key in the dictionary correspond to the pattern variables defined
	in the expressions defined in the definitionPatterns method!"

	className := (aDictionary at: '`className') asSymbol.
	classInstanceVariableNames := self namesIn:
		                              (aDictionary
			                               at: '`#instanceVariableNames'
			                               ifAbsent: [ String new ])
]
