"
I am a refactoring change for a trait addition. Executing this change will create a new trait, just like the ordinary
Trait named: ... call.
At the moment there is no support for the traitcomposition attribute.
"
Class {
	#name : #RBAddTraitChange,
	#superclass : #RBRefactoryDefinitionChange,
	#instVars : [
		'category'
	],
	#category : #'Refactoring-Changes'
}

{ #category : #private }
RBAddTraitChange class >> definitionPatterns [
	^ #('Trait named: `#traitName uses: `@traitComposition category: `#category')
]

{ #category : #converting }
RBAddTraitChange >> asUndoOperation [
	| trait |
	trait := Smalltalk globals
		at: self changeClassName
		ifAbsent: [ nil ].
	^ trait isTrait
		ifTrue: [ RBRefactoryChangeManager changeFactory addTraitDefinition: trait definition ]
		ifFalse: [ RBRefactoryChangeManager changeFactory removeClassNamed: self changeClassName ]
]

{ #category : #accessing }
RBAddTraitChange >> category [
	^ category
]

{ #category : #private }
RBAddTraitChange >> definitionClass [
	^ Trait
]

{ #category : #initialization }
RBAddTraitChange >> fillOutDefinition: aDictionary [
	className := (aDictionary at: '`#traitName') asSymbol.
	category := (aDictionary at: '`#category' ifAbsent: [ #Unclassified ]) asSymbol
]
