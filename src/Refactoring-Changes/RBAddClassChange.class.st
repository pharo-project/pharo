"
I am a refactoring change for a class addition. Executing this change is just like an ordinary class definition by 
calling on ""my superclass"" (or ProtoObject) subclass:....instancevariable... etc.

"
Class {
	#name : 'RBAddClassChange',
	#superclass : 'RBRefactoryDefinitionChange',
	#instVars : [
		'superclassName',
		'instanceVariableNames',
		'classVariableNames',
		'poolDictionaryNames',
		'category'
	],
	#category : 'Refactoring-Changes-Changes-Classes',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Classes'
}

{ #category : 'private' }
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
		'`@superclass weakSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries package: `#category'


		'`@superclass subclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames package: `#category'

		'`@superclass << `#className
			package: `#category'

		'`@superclass << `#className
			slots: `@slots ;
			package: `#category'

		'`@superclass << `#className
			slots: `@slots ;
			sharedVariables: `@sharedVariables ;
			package: `#category'



		'`@superclass << `#className
			sharedVariables:  `@sharedVariables ;
			package: `#category'

		'`@superclass << `#className
			slots: `@slots ;
			sharedPools: `@pools;
			package: `#category'

		'`@superclass << `#className
			sharedPools: `@pools ;
			package: `#category'

		'`@superclass << `#className
			sharedVariables: `@sharedVariables  ;
			sharedPools: `@pools;
			package: `#category'

			'`@superclass << `#className
			slots: `@slots ;
			sharedVariables: `@sharedVariables  ;
			sharedPools: `@pools;
			package: `#category'
		)
		flatCollect: [ :each | Array with: each with: (each copyReplaceAll: '`@superclass' with: 'ProtoObject') , '. `className superclass: `@superclass' ]
]

{ #category : 'converting' }
RBAddClassChange >> asUndoOperation [

	| class |
	class := onSystemDictionary classNamed: self changeClassName.

	^ class isBehavior
		  ifTrue: [ changeFactory addClassDefinition: class oldDefinition ]
		  ifFalse: [ changeFactory removeClassNamed: self changeClassName ]
]

{ #category : 'accessing' }
RBAddClassChange >> category [

	^ category
]

{ #category : 'accessing' }
RBAddClassChange >> classVariableNames [

	^ classVariableNames
]

{ #category : 'private' }
RBAddClassChange >> definitionClass [

	^ onSystemDictionary classNamed:
		  (self superclassName ifNil: [ #ProtoObject ])
]

{ #category : 'private' }
RBAddClassChange >> fillOutDefinition: aDictionary [
	"What a bad method name!!! This has nothing to do with it. It fills up a domain object out of the dictionary."
	"Pay attention that the variables used as key in the dictionary correspond to the pattern variables defined
	in the expressions defined in the definitionPatterns method!"

	superclassName := (aDictionary at: '`@superclass')
		ifNotNil: [ :value | value asSymbol ].
	className := (aDictionary at: '`#className') asSymbol.

	(aDictionary includesKey: '`#instanceVariableNames')
		ifTrue: [
		instanceVariableNames := self namesIn: (aDictionary
			at: '`#instanceVariableNames'
			ifAbsent: [ String new ]).
	].

	(aDictionary includesKey: '`@slots')
		ifTrue: [
		instanceVariableNames :=  (aDictionary
			at: '`@slots') statements collect: [ :each | each value asString ]
	].

	(aDictionary includesKey: '`#classVariableNames')
		ifTrue: [
	classVariableNames := self namesIn: (aDictionary
		at: '`#classVariableNames'
		ifAbsent: [ String new ]).
	].

	(aDictionary includesKey: '`@sharedVariables')
		ifTrue: [
		classVariableNames := (aDictionary at: '`@sharedVariables') statements collect: [ :each | each value asString ]
	].


	(aDictionary includesKey: '`#poolDictionaries')
		ifTrue: [

	poolDictionaryNames := self namesIn: (aDictionary
		at: '`#poolDictionaries'
		ifAbsent: [ String new ]).
	].

	(aDictionary includesKey: '`@pools')
		ifTrue: [

	poolDictionaryNames :=  (aDictionary
			at: '`@pools') statements collect: [ :each | each name ].
	].


	category := (aDictionary
		at: '`#category'
		ifAbsent: [ Class unclassifiedCategory ])
			asSymbol
]

{ #category : 'accessing' }
RBAddClassChange >> instanceVariableNames [

	^ instanceVariableNames
]

{ #category : 'accessing' }
RBAddClassChange >> nameToDisplay [

	^ self changeString
]

{ #category : 'accessing' }
RBAddClassChange >> sharedPoolNames [

	^ poolDictionaryNames
]

{ #category : 'accessing' }
RBAddClassChange >> superclassName [

	^ superclassName
]

{ #category : 'accessing' }
RBAddClassChange >> textToDisplay [

	^ self definition
]
