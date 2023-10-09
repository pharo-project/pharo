"
I represent the declaration of some code to evaluate.  My contents are the code to evaluate.

Sending me the message #import makes me evaluate the code and return the result of the computation.

Be careful, my #import method also handles ChangeSet interaction so far, handling Changeset preambles and Postscripts.
"
Class {
	#name : 'DoItChunk',
	#superclass : 'CodeChunk',
	#category : 'CodeImport-Chunks',
	#package : 'CodeImport',
	#tag : 'Chunks'
}

{ #category : 'visiting' }
DoItChunk >> accept: aVisitor [

	^ aVisitor visitDoItChunk: self
]

{ #category : 'testing' }
DoItChunk >> allowedSelectors [

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
#named:uses:category:
#named:uses:package:
)
]

{ #category : 'importing' }
DoItChunk >> buildClassFromOldDefinition: ast [

	| layoutClass selectorParts superclassName subclassName instanceVariableNames classVariableNames packageName tag poolDictionariesNames traitsDefinition isTrait |
	superclassName := ast receiver formattedCode.
	selectorParts := ast selector findBetweenSubstrings: { $: }.
	isTrait := superclassName endsWith: ' classTrait'.
	layoutClass := FixedLayout.

	self if: selectorParts in: ast includes: #subclass do: [ :argument | subclassName := argument value ].

	self if: selectorParts in: ast includes: #named do: [ :argument |
			subclassName := argument value.
			isTrait := true ].

	self if: selectorParts in: ast includes: #immediateSubclass do: [ :argument |
			subclassName := argument value.
			layoutClass := ImmediateLayout ].

	self if: selectorParts in: ast includes: #variableSubclass do: [ :argument |
			subclassName := argument value.
			layoutClass := VariableLayout ].

	self if: selectorParts in: ast includes: #variableByteSubclass do: [ :argument |
			subclassName := argument value.
			layoutClass := ByteLayout ].

	self if: selectorParts in: ast includes: #variableWordSubclass do: [ :argument |
			subclassName := argument value.
			layoutClass := WordLayout ].

	self if: selectorParts in: ast includes: #weakSubclass do: [ :argument |
			subclassName := argument value.
			layoutClass := WeakLayout ].

	self if: selectorParts in: ast includes: #instanceVariableNames do: [ :argument | instanceVariableNames := argument value ].

	self if: selectorParts in: ast includes: #classVariableNames do: [ :argument | classVariableNames := argument value ].

	self if: selectorParts in: ast includes: #category do: [ :argument |
			"This is not right but we cannot rely on categories anymore and this is for backward compatibility."
			| aString |
			aString := argument value.
			(aString includes: $-)
				ifTrue: [
					packageName := aString copyUpToLast: $-.
					tag := aString copyAfterLast: $- ]
				ifFalse: [ packageName := aString ] ].

	self if: selectorParts in: ast includes: #package do: [ :argument | packageName := argument value ].

	self if: selectorParts in: ast includes: #poolDictionaries do: [ :argument | poolDictionariesNames := argument value ].

	self if: selectorParts in: ast includes: #layout do: [ :argument |
		layoutClass := self class environment at: argument formattedCode ifAbsent: [ self error: 'Layout ' , argument formattedCode , ' not found.' ] ].

	self if: selectorParts in: ast includes: #uses do: [ :argument | traitsDefinition := argument formattedCode ].

	(#( #instanceVariableNames: uses: uses:instanceVariableNames: ) includes: ast selector) ifTrue: [
		subclassName := ast receiver formattedCode asSymbol.
		superclassName := nil ].

	(#( CompiledBlock CompiledCode CompiledMethod ) includes: subclassName) ifTrue: [ layoutClass := CompiledMethodLayout ].

	^ self class classInstaller make: [ :aBuilder |
		  | superclass |
		  superclass := self class environment at: superclassName asSymbol.
		  aBuilder
			  superclass: superclass;
			  environment: superclass environment;
			  name: subclassName;
			  layoutClass: layoutClass.
		  instanceVariableNames ifNotNil: [ aBuilder slotsFromString: instanceVariableNames ].
		  classVariableNames ifNotNil: [ aBuilder sharedVariablesFromString: classVariableNames ].
		  packageName ifNotNil: [ aBuilder package: packageName ].
		  tag ifNotNil: [ aBuilder tag: tag ].
		  poolDictionariesNames ifNotNil: [ aBuilder sharedPools: poolDictionariesNames ].
		  isTrait ifTrue: [ aBuilder beTrait ].
		  traitsDefinition ifNotNil: [ aBuilder traitComposition: traitsDefinition ] ]
]

{ #category : 'accessing' }
DoItChunk >> description [

	^ 'DoIt (', (contents first: (40 min: contents size)), ')'
]

{ #category : 'private' }
DoItChunk >> if: selectorParts in: ast includes: aSymbol do: aBlock [

	| index |
	index := selectorParts indexOf: aSymbol ifAbsent: [ ^ self ].
	aBlock value: (ast arguments at: index)
]

{ #category : 'importing' }
DoItChunk >> importFor: requestor logSource: logSource [

	| ast |
	(contents beginsWith: '----') ifTrue: [ ^ self ].

	SystemAnnouncer announce: (DoItChunkImported new
			 contents: contents;
			 logSource: logSource;
			 yourself).

	"The package and class creation code got updated in Pharo but Monticello rely on this code. In order to not break the compatibility we are now checking if the code to import is this code and we handle it in another way."
	ast := RBParser parseExpression: contents.
	(self isPackageAddition: ast) ifTrue: [ ^ self packageOrganizer ensurePackageMatching: ast arguments first value ].
	(self isOldClassCreation: ast) ifTrue: [ ^ self buildClassFromOldDefinition: ast ].

	^ Smalltalk compiler class new
		  source: contents;
		  requestor: requestor;
		  logged: logSource;
		  evaluate
]

{ #category : 'testing' }
DoItChunk >> isDoItDeclaration [

	^ true
]

{ #category : 'importing' }
DoItChunk >> isOldClassCreation: ast [
	"If this is true, that means this is the old class definition."

	^ ast isMessage and: [ self allowedSelectors includes: ast selector ]
]

{ #category : 'testing' }
DoItChunk >> isPackageAddition: ast [
	"This is for backward compatibility to be able to import code with old way of creating a package."

	^ ast isMessage and: [ ast receiver isVariable and: [ ast receiver name = #SystemOrganization and: [ ast selector = #addCategory: ] ] ]
]
