"
I am a refactoring change for a class addition. Executing this change is just like an ordinary class definition by 
calling on ""my superclass"" (or ProtoObject) subclass:....instancevariable... etc.

"
Class {
	#name : #RBAddClassChange,
	#superclass : #RBRefactoryDefinitionChange,
	#instVars : [
		'superclassName',
		'instanceVariableNames',
		'classVariableNames',
		'poolDictionaryNames',
		'category'
	],
	#category : #'Refactoring-Changes'
}

{ #category : #private }
RBAddClassChange class >> definitionPatterns [
	^ #('`@superclass subclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass subclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableByteSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableByteSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableWordSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableWordSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass weakSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass weakSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass subclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category'
		'`@superclass subclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category'
		'`@superclass variableByteSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category'
		'`@superclass variableByteSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category'
		'`@superclass variableSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category'
		'`@superclass variableSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category'
		'`@superclass variableWordSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category'
		'`@superclass variableWordSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category'
		'`@superclass weakSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category'
		'`@superclass weakSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category')
		flatCollect: [ :each | Array with: each with: (each copyReplaceAll: '`@superclass' with: 'ProtoObject') , '. `className superclass: `@superclass' ]
]

{ #category : #converting }
RBAddClassChange >> asUndoOperation [
	| class |
	class := onSystemDictionary 
					classNamed: self changeClassName.

	^ class isBehavior
		ifTrue: [ changeFactory addClassDefinition: class oldDefinition ]
		ifFalse: [ changeFactory removeClassNamed: self changeClassName ]
]

{ #category : #accessing }
RBAddClassChange >> category [
	^ category
]

{ #category : #accessing }
RBAddClassChange >> classVariableNames [
	^ classVariableNames
]

{ #category : #private }
RBAddClassChange >> definitionClass [
	^ onSystemDictionary classNamed: (self superclassName ifNil: [ #ProtoObject ])
]

{ #category : #private }
RBAddClassChange >> fillOutDefinition: aDictionary [
	superclassName := (aDictionary at: '`@superclass')
		ifNotNil: [ :value | value asSymbol ].
	className := (aDictionary at: '`#className') asSymbol.
	instanceVariableNames := self namesIn: (aDictionary
		at: '`#instanceVariableNames'
		ifAbsent: [ String new ]).
	classVariableNames := self namesIn: (aDictionary
		at: '`#classVariableNames'
		ifAbsent: [ String new ]).
	poolDictionaryNames := self namesIn: (aDictionary
		at: '`#poolDictionaries'
		ifAbsent: [ String new ]).
	category := (aDictionary
		at: '`#category'
		ifAbsent: [ #Unclassified ])
			asSymbol
]

{ #category : #accessing }
RBAddClassChange >> instanceVariableNames [
	^ instanceVariableNames
]

{ #category : #accessing }
RBAddClassChange >> nameToDisplay [

	^ self changeString
]

{ #category : #accessing }
RBAddClassChange >> sharedPoolNames [
	^ poolDictionaryNames
]

{ #category : #accessing }
RBAddClassChange >> superclassName [
	^ superclassName
]

{ #category : #accessing }
RBAddClassChange >> textToDisplay [

	^ self definition
]
