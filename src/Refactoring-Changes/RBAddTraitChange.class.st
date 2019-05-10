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
	^ #('Trait named: `#traitName uses: `@traitComposition package: `#package' 'Trait named: `#traitName uses: `@traitComposition slots: `@slotDefinition package: `#package' )
]

{ #category : #converting }
RBAddTraitChange >> asUndoOperation [
	| trait |
	trait := onSystemDictionary 
		at: self changeClassName
		ifAbsent: [ nil ].
	^ trait isTrait
		ifTrue: [ changeFactory addTraitDefinition: trait definition ]
		ifFalse: [ changeFactory removeClassNamed: self changeClassName ]
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
	category := (aDictionary at: '`#package' ifAbsent: [ #Unclassified ]) asSymbol
]
