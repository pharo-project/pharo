"
A MCClassDefinition represents a class.
"
Class {
	#name : #MCClassDefinition,
	#superclass : #MCDefinition,
	#instVars : [
		'name',
		'superclassName',
		'variables',
		'category',
		'type',
		'comment',
		'commentStamp',
		'traitComposition',
		'classTraitComposition'
	],
	#category : #'Monticello-Modeling'
}

{ #category : #obsolete }
MCClassDefinition class >> name: nameString
superclassName: superclassString
category: categoryString 
instVarNames: ivarArray
classVarNames: cvarArray
poolDictionaryNames: poolArray
classInstVarNames: civarArray
comment: commentString [
	^ self	name: nameString
			superclassName: superclassString
			category: categoryString 
			instVarNames: ivarArray
			classVarNames: cvarArray
			poolDictionaryNames: poolArray
			classInstVarNames: civarArray
			type: #normal
			comment: commentString

]

{ #category : #obsolete }
MCClassDefinition class >> name: nameString
superclassName: superclassString
category: categoryString 
instVarNames: ivarArray
classVarNames: cvarArray
poolDictionaryNames: poolArray
classInstVarNames: civarArray
type: typeSymbol
comment: commentString [
	^ self 
		name: nameString
		superclassName: superclassString
		category: categoryString 
		instVarNames: ivarArray
		classVarNames: cvarArray
		poolDictionaryNames: poolArray
		classInstVarNames: civarArray
		type: typeSymbol
		comment: commentString
		commentStamp: nil
]

{ #category : #'instance creation' }
MCClassDefinition class >> name: nameString
superclassName: superclassString
category: categoryString 
instVarNames: ivarArray
classVarNames: cvarArray
poolDictionaryNames: poolArray
classInstVarNames: civarArray
type: typeSymbol
comment: commentString
commentStamp: stampString [
	^ self instanceLike:
		(self new initializeWithName: nameString
					superclassName: superclassString
					traitComposition: '{}'
					classTraitComposition: '{}'
					category: categoryString 
					instVarNames: ivarArray
					classVarNames: cvarArray
					poolDictionaryNames: poolArray
					classInstVarNames: civarArray
					type: typeSymbol
					comment: commentString
					commentStamp: stampString)
]

{ #category : #obsolete }
MCClassDefinition class >> name: nameString
superclassName: superclassString
category: categoryString 
instVarNames: ivarArray
comment: commentString [
	^ self	name: nameString
			superclassName: superclassString
			category: categoryString 
			instVarNames: ivarArray
			classVarNames: #()
			poolDictionaryNames: #()
			classInstVarNames: #()
			comment: commentString

]

{ #category : #'instance creation' }
MCClassDefinition class >> name: nameString
superclassName: superclassString
traitComposition: traitCompositionString
classTraitComposition: classTraitCompositionString
category: categoryString 
instVarNames: ivarArray
classVarNames: cvarArray
poolDictionaryNames: poolArray
classInstVarNames: civarArray
type: typeSymbol
comment: commentString
commentStamp: stampString [
	
	^ self instanceLike:
		(self new initializeWithName: nameString
					superclassName: superclassString
					traitComposition: traitCompositionString
					classTraitComposition: classTraitCompositionString
					category: categoryString 
					instVarNames: ivarArray
					classVarNames: cvarArray
					poolDictionaryNames: poolArray
					classInstVarNames: civarArray
					type: typeSymbol
					comment: commentString
					commentStamp: stampString)
]

{ #category : #comparing }
MCClassDefinition >> = aDefinition [
	^(super = aDefinition)
		and: [superclassName = aDefinition superclassName
		and: [self traitCompositionString = aDefinition traitCompositionString
		and: [self classTraitCompositionString = aDefinition classTraitCompositionString
		and: [category = aDefinition category
		and: [type = aDefinition type
		and: [self sortedVariables = aDefinition sortedVariables
		and: [comment = aDefinition comment]]]]]]]
]

{ #category : #visiting }
MCClassDefinition >> accept: aVisitor [
	aVisitor visitClassDefinition: self.
	(self hasClassInstanceVariables or: [self hasClassTraitComposition])
		ifTrue: [aVisitor visitMetaclassDefinition: self].

]

{ #category : #accessing }
MCClassDefinition >> actualClass [
	^ Smalltalk globals classNamed: self className
]

{ #category : #initializing }
MCClassDefinition >> addVariables: aCollection ofType: aClass [
	variables addAll: (aCollection collect: [:var | aClass name: var asString]).
]

{ #category : #accessing }
MCClassDefinition >> category [
	^ category
]

{ #category : #accessing }
MCClassDefinition >> classDefinitionString [
	"Answer a string describing the class-side definition."
	
	^String streamContents: [:stream | self printClassDefinitionOn: stream]
]

{ #category : #accessing }
MCClassDefinition >> classInstVarNames [
	^ self selectVariables: #isClassInstanceVariable
]

{ #category : #printing }
MCClassDefinition >> classInstanceVariables [
	^self usesSpecialVariables 
		ifTrue: [ self variablesOfType: #isClassInstanceVariable]
		ifFalse: [self classInstanceVariablesString asSlotCollection]
]

{ #category : #printing }
MCClassDefinition >> classInstanceVariablesString [
	^ self stringForVariablesOfType: #isClassInstanceVariable
]

{ #category : #accessing }
MCClassDefinition >> className [
	^ name
]

{ #category : #accessing }
MCClassDefinition >> classTraitComposition [
	^classTraitComposition
]

{ #category : #accessing }
MCClassDefinition >> classTraitCompositionCompiled [
		^(Smalltalk compiler evaluate: self classTraitCompositionString) asTraitComposition 
]

{ #category : #accessing }
MCClassDefinition >> classTraitCompositionString [
	^self classTraitComposition ifNil: ['{}'].
]

{ #category : #accessing }
MCClassDefinition >> classVarNames [
	^(self selectVariables: #isClassVariable) asArray sort
]

{ #category : #printing }
MCClassDefinition >> classVariables [
	^self usesSpecialVariables 
		ifTrue: [self sortedVariablesOfType: #isClassVariable ]
		ifFalse: [(self classVariablesString substrings: ' ') collect: [:x | x asSymbol => ClassVariable]].

]

{ #category : #printing }
MCClassDefinition >> classVariablesString [
	^ self stringForSortedVariablesOfType: #isClassVariable
]

{ #category : #accessing }
MCClassDefinition >> comment [
	^ comment
]

{ #category : #accessing }
MCClassDefinition >> commentStamp [
	^ commentStamp
]

{ #category : #installing }
MCClassDefinition >> createClass [
	| superClass |
	"Ignore Context definition because of troubles with class migration on bootstrapped image. Temporary solution."
	name = #Context
		ifTrue: [ Context comment = comment 
						ifFalse: [ Context comment: comment stamp: commentStamp ].
					^ self ].
	superClass := superclassName == #nil
		ifFalse: [ Smalltalk globals at: superclassName ].
	^ [ Smalltalk classInstaller
		make: [ :builder | 
			builder
				superclass: superClass;
				name: name;
				layoutClass: (ObjectLayout layoutForType: type);
				slots: self instanceVariables;
				sharedVariables: self classVariables;
				sharedPools: self sharedPoolsString;
				classSlots: self classInstanceVariables;
				traitComposition: self traitCompositionCompiled;
				classTraitComposition: self classTraitCompositionCompiled;
				comment: comment stamp: commentStamp;
				category: category;
				environment: superClass environment ] ]
		on: Warning , DuplicatedVariableError
		do: [ :ex | ex resume ]
]

{ #category : #initializing }
MCClassDefinition >> defaultCommentStamp [
	^ String new

	"The version below avoids stomping on stamps already in the image

	^ (Smalltalk globals at: name ifPresent: [:c | c organization commentStamp])
		ifNil: ['']
	"

]

{ #category : #printing }
MCClassDefinition >> definitionString [
	^ String streamContents: [:stream | self printDefinitionOn: stream]
]

{ #category : #accessing }
MCClassDefinition >> description [
	^ Array with: name

]

{ #category : #testing }
MCClassDefinition >> hasClassInstanceVariables [
	^ (self selectVariables: #isClassInstanceVariable) isEmpty not
]

{ #category : #testing }
MCClassDefinition >> hasClassTraitComposition [
	^self classTraitCompositionString ~= '{}'
]

{ #category : #testing }
MCClassDefinition >> hasComment [
	^ comment isEmptyOrNil not
]

{ #category : #testing }
MCClassDefinition >> hasTraitComposition [
	^self traitCompositionString ~= '{}'
]

{ #category : #comparing }
MCClassDefinition >> hash [
	| hash |
	hash := String stringHash: name initialHash: 0.
	hash := String stringHash: superclassName initialHash: hash.
	hash := String stringHash: self traitCompositionString initialHash: hash.
	hash := String stringHash: self classTraitComposition asString initialHash: hash.
	hash := String stringHash: (category ifNil: ['']) initialHash: hash.
	hash := String stringHash: type initialHash: hash.
	variables do: [
		:v |
		hash := String stringHash: v name initialHash: hash.
	].
	^ hash
]

{ #category : #initializing }
MCClassDefinition >> initializeWithName: nameString
superclassName: superclassString
category: categoryString 
instVarNames: ivarArray
classVarNames: cvarArray
poolDictionaryNames: poolArray
classInstVarNames: civarArray
type: typeSymbol
comment: commentString
commentStamp: stampStringOrNil [
	name := nameString asSymbol.
	superclassName := superclassString ifNil: ['nil'] ifNotNil: [superclassString asSymbol].
	category := categoryString.
	name = #CompiledMethod ifTrue: [type := #compiledMethod] ifFalse: [type := typeSymbol].
	comment := commentString withSqueakLineEndings.
	commentStamp := stampStringOrNil ifNil: [self defaultCommentStamp].
	variables := OrderedCollection  new.
	self addVariables: ivarArray ofType: MCInstanceVariableDefinition.
	self addVariables: cvarArray asSortedCollection ofType: MCClassVariableDefinition.
	self addVariables: poolArray asSortedCollection ofType: MCPoolImportDefinition.
	self addVariables: civarArray ofType: MCClassInstanceVariableDefinition.
]

{ #category : #initializing }
MCClassDefinition >> initializeWithName: nameString
superclassName: superclassString
traitComposition: traitCompositionString
classTraitComposition: classTraitCompositionString
category: categoryString 
instVarNames: ivarArray
classVarNames: cvarArray
poolDictionaryNames: poolArray
classInstVarNames: civarArray
type: typeSymbol
comment: commentString
commentStamp: stampStringOrNil [
	name := nameString asSymbol.
	superclassName := superclassString ifNil: ['nil'] ifNotNil: [superclassString asSymbol].
	traitComposition := traitCompositionString.
	classTraitComposition := classTraitCompositionString.
	category := categoryString.
	name = #CompiledMethod ifTrue: [type := #compiledMethod] ifFalse: [type := typeSymbol].
	comment := commentString withSqueakLineEndings.
	commentStamp := stampStringOrNil ifNil: [self defaultCommentStamp].
	variables := OrderedCollection  new.
	self addVariables: ivarArray ofType: MCInstanceVariableDefinition.
	self addVariables: cvarArray asSortedCollection ofType: MCClassVariableDefinition.
	self addVariables: poolArray asSortedCollection ofType: MCPoolImportDefinition.
	self addVariables: civarArray ofType: MCClassInstanceVariableDefinition.
]

{ #category : #accessing }
MCClassDefinition >> instVarNames [
	^ self selectVariables: #isInstanceVariable
]

{ #category : #printing }
MCClassDefinition >> instanceVariables [
	^self usesSpecialVariables 
		ifTrue: [self variablesOfType: #isInstanceVariable]
		ifFalse: [self instanceVariablesString asSlotCollection]
]

{ #category : #printing }
MCClassDefinition >> instanceVariablesString [
	^ self stringForVariablesOfType: #isInstanceVariable
]

{ #category : #testing }
MCClassDefinition >> isClassDefinition [
	^ true
]

{ #category : #testing }
MCClassDefinition >> isCodeDefinition [
	^ true
]

{ #category : #printing }
MCClassDefinition >> kindOfSubclass [
	type = #normal ifTrue: [^ ' subclass: '].
	type = #words ifTrue: [^ ' variableWordSubclass: '].
	type = #variable ifTrue: [^ ' variableSubclass: '].
	type = #bytes ifTrue: [^ ' variableByteSubclass: '].
	type = #weak ifTrue: [^ ' weakSubclass: ' ].
	type = #compiledMethod ifTrue: [^ ' variableByteSubclass: ' ].
	type = #immediate ifTrue: [ ^ ' immediateSubclass: ' ].
	type = #ephemeron ifTrue: [ ^ ' ephemeronSubclass: ' ].
	"hack to support user defined layouts"
	(Smalltalk hasClassNamed: type) ifTrue: [ ^type asString].
	self error: 'Unrecognized class type'
]

{ #category : #installing }
MCClassDefinition >> load [
	self createClass
]

{ #category : #accessing }
MCClassDefinition >> poolDictionaries [
	^ self selectVariables: #isPoolImport
]

{ #category : #annotations }
MCClassDefinition >> printAnnotations: requests on: aStream [
	"Add a string for an annotation pane, trying to fulfill the browser annotationRequests"

	requests do: [ :aRequest |
		aRequest == #requirements ifTrue: [
			self requirements do: [ :req | aStream nextPutAll: req ] separatedBy: [ aStream space ]]
	] separatedBy: [ aStream space ].
]

{ #category : #printing }
MCClassDefinition >> printClassDefinitionOn: stream [
	"Print a class-side definition of the receiver on the given stream.
	Class instance variables and class traits."
	
		stream
			nextPutAll: self className;
			nextPutAll: ' class';
			cr; tab.
		self hasClassTraitComposition ifTrue: [
			stream 
				nextPutAll: 'uses: ';
		 		nextPutAll: self classTraitCompositionString;
				cr; tab ].
		stream
			nextPutAll: 'instanceVariableNames: ';
			store: self classInstanceVariablesString
]

{ #category : #printing }
MCClassDefinition >> printDefinitionOn: stream [
		stream 
			nextPutAll: self superclassName;
			nextPutAll: self kindOfSubclass;
			nextPut: $# ;
			nextPutAll: self className;
			cr; tab.
		self hasTraitComposition ifTrue: [
			stream 
				nextPutAll: 'uses: ';
		 		nextPutAll: self traitCompositionString;
				cr; tab ].
		stream
			nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString;
			cr; tab;
			nextPutAll: 'classVariableNames: ';
			store: self classVariablesString;
			cr; tab;
			nextPutAll: 'poolDictionaries: ';
			store: self sharedPoolsString;
			cr; tab;
			nextPutAll: 'category: ';
			store: self category asString
]

{ #category : #printing }
MCClassDefinition >> printMetaDefinitionOn: stream [
	stream 
		nextPutAll: self className;
		nextPutAll: ' class'; cr; tab.
	self hasClassTraitComposition ifTrue: [
		stream
			nextPutAll: 'uses: ';
			nextPutAll: self classTraitCompositionString;
			cr; tab ].
	stream
		nextPutAll: ' instanceVariableNames: '; 
		store: self classInstanceVariablesString
]

{ #category : #comparing }
MCClassDefinition >> provisions [
	^ Array with: name
]

{ #category : #comparing }
MCClassDefinition >> requirements [
	^ ((superclassName == #nil) or: [superclassName asString beginsWith: 'AnObsolete'])
		ifTrue: [self poolDictionaries]
		ifFalse: [(Array with: superclassName), self poolDictionaries]
]

{ #category : #accessing }
MCClassDefinition >> selectVariables: aSelector [
	^ variables select: [:v | v perform: aSelector] thenCollect: [:v | v name]
]

{ #category : #printing }
MCClassDefinition >> sharedPoolsString [
	^ self stringForSortedVariablesOfType: #isPoolImport
]

{ #category : #accessing }
MCClassDefinition >> sortKey [
	^ self className
]

{ #category : #accessing }
MCClassDefinition >> sortedVariables [
	"sort variables for comparison purposes"

	| sorted |
	sorted := variables select: [:var | var isOrderDependend].
	sorted addAll: ((variables reject: [:var | var isOrderDependend])
		asSortedCollection: [:a :b | a name <= b name]).
	^sorted
]

{ #category : #installing }
MCClassDefinition >> sortedVariablesOfType: aSymbol [
	"version for complex vars, { definition . definition }"
	^(((self selectVariables: aSymbol) asSortedCollection collect: [:each | Smalltalk compiler evaluate: each])) asArray.
		
]

{ #category : #printing }
MCClassDefinition >> source [
	^ self definitionString
]

{ #category : #storing }
MCClassDefinition >> storeDataOn: aDataStream [
	| instVarSize |
	instVarSize := (self hasTraitComposition or: [ self hasClassTraitComposition ])
		ifTrue: [ self class instSize ]
		ifFalse: [ self class instSize - 2 ].
	aDataStream
		beginInstance: self class
		size: instVarSize.
	1 to: instVarSize do: [ :index |
		aDataStream nextPut: (self instVarAt: index) ].
]

{ #category : #installing }
MCClassDefinition >> stringForSortedVariablesOfType: aSymbol [
	^ String streamContents:
		[:stream |
		(self selectVariables: aSymbol) asSortedCollection
			do: [:ea | stream nextPutAll: ea]
			separatedBy: [stream space]]
]

{ #category : #installing }
MCClassDefinition >> stringForVariablesOfType: aSymbol [
	^ String streamContents:
		[:stream |
		(self selectVariables: aSymbol) 
			do: [:ea | stream nextPutAll: ea]
			separatedBy: [stream space]]
]

{ #category : #printing }
MCClassDefinition >> summary [
	^ name
]

{ #category : #accessing }
MCClassDefinition >> superclassName [
	^ superclassName
]

{ #category : #accessing }
MCClassDefinition >> traitComposition [
	^traitComposition
]

{ #category : #accessing }
MCClassDefinition >> traitCompositionCompiled [
	^(Smalltalk compiler evaluate: self traitCompositionString) asTraitComposition 
]

{ #category : #accessing }
MCClassDefinition >> traitCompositionString [
	^self traitComposition ifNil: ['{}'].
]

{ #category : #accessing }
MCClassDefinition >> type [
	^ type
]

{ #category : #installing }
MCClassDefinition >> unload [
	Smalltalk globals removeClassNamed: name
]

{ #category : #installing }
MCClassDefinition >> usesSpecialVariables [
	"this checks if any ivar or class var is using more than just standard definitions.
	Complex vars are encoded with a string that starts with a # or one that has a space"
	
	^self variables anySatisfy: [:var | (var name beginsWith:'#') or: [ var name includes: Character space ]]
]

{ #category : #accessing }
MCClassDefinition >> variables [
	^ variables
]

{ #category : #installing }
MCClassDefinition >> variablesOfType: aSymbol [
	"version for complex vars, { definition . definition }"
	^(((self selectVariables: aSymbol) collect: [:each | Smalltalk compiler evaluate: each])) asArray.
		
]
