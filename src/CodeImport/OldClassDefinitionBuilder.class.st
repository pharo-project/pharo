"
I am a class whose goal is to create and install classes in Pharo from old class definition chunks without executing the old class definition API.
Instead I'm using the AST of this old definition to get the arguments I need and create a shift class builder.
"
Class {
	#name : 'OldClassDefinitionBuilder',
	#superclass : 'Object',
	#instVars : [
		'ast'
	],
	#category : 'CodeImport-Chunks',
	#package : 'CodeImport',
	#tag : 'Chunks'
}

{ #category : 'accessing' }
OldClassDefinitionBuilder class >> allowedSelectors [

	^ #(
#immediateSubclass:instanceVariableNames:classVariableNames:package:
#immediateSubclass:instanceVariableNames:classVariableNames:category:
#immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:package:
#immediateSubclass:uses:instanceVariableNames:classVariableNames:package:
#immediateSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:package:

#subclass:
#subclass:instanceVariableNames:
#subclass:instanceVariableNames:classVariableNames:category:
#subclass:instanceVariableNames:classVariableNames:package:
#subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
#subclass:instanceVariableNames:classVariableNames:poolDictionaries:package:

#subclass:uses:
#subclass:uses:instanceVariableNames:classVariableNames:category:
#subclass:uses:instanceVariableNames:classVariableNames:package:
#subclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:category:
#subclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:package:
#subclass:uses:layout:slots:classVariables:category:
#subclass:uses:layout:slots:classVariables:poolDictionaries:category:
#subclass:uses:slots:classVariables:category:
#subclass:uses:slots:classVariables:poolDictionaries:category:

#variableByteSubclass:instanceVariableNames:classVariableNames:category:
#variableByteSubclass:instanceVariableNames:classVariableNames:package:
#variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
#variableByteSubclass:uses:instanceVariableNames:classVariableNames:category:
#variableByteSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:category:

#variableSubclass:instanceVariableNames:classVariableNames:category:
#variableSubclass:instanceVariableNames:classVariableNames:package:
#variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
#variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:package:
#variableSubclass:uses:instanceVariableNames:classVariableNames:category:
#variableSubclass:uses:instanceVariableNames:classVariableNames:package:
#variableSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:category:
#variableSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:package:

#variableWordSubclass:instanceVariableNames:classVariableNames:category:
#variableWordSubclass:instanceVariableNames:classVariableNames:package:
#variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
#variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:package:
#variableWordSubclass:uses:instanceVariableNames:classVariableNames:category:
#variableWordSubclass:uses:instanceVariableNames:classVariableNames:package:
#variableWordSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:category:
#variableWordSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:package:

#weakSubclass:instanceVariableNames:classVariableNames:category:
#weakSubclass:instanceVariableNames:classVariableNames:package:
#weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
#weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:package:
#weakSubclass:uses:instanceVariableNames:classVariableNames:category:
#weakSubclass:uses:instanceVariableNames:classVariableNames:package:
#weakSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:category:
#weakSubclass:uses:instanceVariableNames:classVariableNames:poolDictionaries:package:

#named:
#named:uses:
#named:package:
#named:uses:category:
#named:uses:package:
#named:uses:category:env:
#named:uses:package:env:
#named:uses:slots:category:
#named:uses:slots:package:
#named:uses:slots:package:env:
#named:uses:slots:category:env:
#named:instanceVariableNames:package:
#named:uses:instanceVariableNames:package:
#named:uses:instanceVariableNames:package:env:
#uses:instanceVariableNames:
)
]

{ #category : 'building' }
OldClassDefinitionBuilder class >> buildFromAST: ast [

	^ self new
		  ast: ast;
		  build
]

{ #category : 'testing' }
OldClassDefinitionBuilder class >> isOldClassCreation: ast [
	"I take as parameter an AST and return true if it matches an old class definition"

	^ ast isMessage and: [ self allowedSelectors includes: ast selector ]
]

{ #category : 'accessing' }
OldClassDefinitionBuilder >> ast [

	^ ast
]

{ #category : 'accessing' }
OldClassDefinitionBuilder >> ast: anObject [

	ast := anObject
]

{ #category : 'building' }
OldClassDefinitionBuilder >> build [

	| layoutClass selectorParts superclassName subclassName instanceVariableNames classVariableNames packageName poolDictionariesNames traitsDefinition isTrait |
	
	superclassName := ast receiver formattedCode.
	selectorParts := ast selector findBetweenSubstrings: { $: }.
	isTrait := superclassName endsWith: ' classTrait'.
	
	ast selector == #uses:instanceVariableNames: ifTrue: [ | trait updatedTrait|
	"this case does not fit in the scheme used later, we treat it specially here"
	
	trait :=  self class compiler evaluate: superclassName.
	self if: selectorParts includes: #uses do: [ :argument | traitsDefinition := argument value ].
	self if: selectorParts includes: #instanceVariableNames do: [ :argument | instanceVariableNames := argument value ].
	
	updatedTrait := trait instanceSide classInstaller update: trait instanceSide to: [ :aBuilder |
		  aBuilder
			  environment: trait environment;
			  	classTraitComposition: (self class compiler evaluate: traitsDefinition formattedCode);
				classSlots: instanceVariableNames.
			isTrait ifTrue: [ aBuilder beTrait ]]. 
	^ updatedTrait classSide	
	].
	layoutClass := FixedLayout.

	self if: selectorParts includes: #subclass do: [ :argument | subclassName := argument value ].

	self if: selectorParts includes: #named do: [ :argument |
		subclassName := argument value.
		isTrait := true ].

	self if: selectorParts includes: #immediateSubclass do: [ :argument |
		subclassName := argument value.
		layoutClass := ImmediateLayout ].

	self if: selectorParts includes: #variableSubclass do: [ :argument |
		subclassName := argument value.
		layoutClass := VariableLayout ].

	self if: selectorParts includes: #variableByteSubclass do: [ :argument |
		subclassName := argument value.
		layoutClass := ByteLayout ].

	self if: selectorParts includes: #variableWordSubclass do: [ :argument |
		subclassName := argument value.
		layoutClass := WordLayout ].

	self if: selectorParts includes: #weakSubclass do: [ :argument |
		subclassName := argument value.
		layoutClass := WeakLayout ].

	self if: selectorParts includes: #instanceVariableNames do: [ :argument | instanceVariableNames := argument value ].

	self if: selectorParts includes: #classVariableNames do: [ :argument | classVariableNames := argument value ].

	self if: selectorParts includes: #category do: [ :argument | packageName := argument value ].

	self if: selectorParts includes: #package do: [ :argument | packageName := argument value ].

	self if: selectorParts includes: #poolDictionaries do: [ :argument | poolDictionariesNames := argument value ].

	self
		if: selectorParts
		includes: #layout
		do: [ :argument |
		layoutClass := self class environment at: argument formattedCode ifAbsent: [ self error: 'Layout ' , argument formattedCode , ' not found.' ] ].

	self if: selectorParts includes: #uses do: [ :argument | traitsDefinition := argument formattedCode ].

	(#( #instanceVariableNames: uses: uses:instanceVariableNames: ) includes: ast selector) ifTrue: [
		subclassName := ast receiver formattedCode asSymbol.
		superclassName := nil ].

	(#( CompiledBlock CompiledCode CompiledMethod ) includes: subclassName) ifTrue: [ layoutClass := CompiledMethodLayout ].

	^ self class classInstaller make: [ :aBuilder |
		  | superclass converter |
		  superclass := self class environment at: superclassName asSymbol.

		  aBuilder
			  superclass: superclass;
			  environment: superclass environment;
			  name: subclassName;
			  layoutClass: layoutClass.
		  instanceVariableNames ifNotNil: [ aBuilder slotsFromString: instanceVariableNames ].
		  classVariableNames ifNotNil: [ aBuilder sharedVariablesFromString: classVariableNames ].
		
		  converter := CategoryConverter category: packageName environment: superclass environment.
		  aBuilder package: converter packageName.
		  aBuilder tag: converter tagName.

		  poolDictionariesNames ifNotNil: [ aBuilder sharedPoolsFromString: poolDictionariesNames ].
		  isTrait ifTrue: [ aBuilder beTrait ].
		  traitsDefinition ifNotNil: [ aBuilder traitComposition: (self class compiler evaluate: traitsDefinition) ] ]
]

{ #category : 'private' }
OldClassDefinitionBuilder >> if: selectorParts includes: aSymbol do: aBlock [

	| index |
	index := selectorParts indexOf: aSymbol ifAbsent: [ ^ self ].
	aBlock value: (ast arguments at: index)
]
