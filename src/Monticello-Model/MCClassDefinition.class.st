"
A MCClassDefinition represents a class.
"
Class {
	#name : 'MCClassDefinition',
	#superclass : 'MCDefinition',
	#instVars : [
		'name',
		'superclassName',
		'variables',
		'packageName',
		'type',
		'comment',
		'commentStamp',
		'tagName',
		'traitComposition',
		'classTraitComposition'
	],
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'instance creation' }
MCClassDefinition class >> named: nameString [

	^ self new
		  name: nameString;
		  yourself
]

{ #category : 'comparing' }
MCClassDefinition >> = aDefinition [
	^ super = aDefinition
		and: [ superclassName = aDefinition superclassName
		and: [ self traitCompositionString = aDefinition traitCompositionString
		and: [ self classTraitCompositionString = aDefinition classTraitCompositionString
		and: [ self packageName = aDefinition packageName
		and: [  self tagName = aDefinition tagName
		and: [ type = aDefinition type and: [ self sortedVariables = aDefinition sortedVariables and: [ comment = aDefinition comment ] ] ] ] ] ] ]]
]

{ #category : 'visiting' }
MCClassDefinition >> accept: aVisitor [
	aVisitor visitClassDefinition: self.
	(self hasClassInstanceVariables or: [self hasClassTraitComposition])
		ifTrue: [aVisitor visitMetaclassDefinition: self].

]

{ #category : 'accessing' }
MCClassDefinition >> actualClass [

	^ self class environment classNamed: self className
]

{ #category : 'initialization' }
MCClassDefinition >> addVariables: aCollection ofType: aClass [

	variables addAll: (aCollection collect: [ :var | aClass name: var asString ])
]

{ #category : 'accessing' }
MCClassDefinition >> category [

	^ self tagName
		  ifNil: [ self packageName ]
		  ifNotNil: [ :tag | self packageName , '-' , tag ]
]

{ #category : 'accessing' }
MCClassDefinition >> category: categoryString [

	self packageName: categoryString
]

{ #category : 'accessing' }
MCClassDefinition >> classDefinitionString [
	"Answer a string describing the class-side definition."
	
	^String streamContents: [:stream | self printClassDefinitionOn: stream]
]

{ #category : 'accessing' }
MCClassDefinition >> classInstVarNames [
	^ self selectVariables: #isClassInstanceVariable
]

{ #category : 'accessing' }
MCClassDefinition >> classInstVarNames: civarArray [

	self addVariables: civarArray ofType: MCClassInstanceVariableDefinition
]

{ #category : 'printing' }
MCClassDefinition >> classInstanceVariables [

	variables ifNil: [ ^ #() ].

	^self needsSlotClassDefinition 
		ifTrue: [ self variablesOfType: #isClassInstanceVariable]
		ifFalse: [self classInstanceVariablesString asSlotCollection]
]

{ #category : 'printing' }
MCClassDefinition >> classInstanceVariablesString [

	^ self stringForVariablesOfType: #isClassInstanceVariable
]

{ #category : 'accessing' }
MCClassDefinition >> className [
	^ name
]

{ #category : 'accessing' }
MCClassDefinition >> classTraitComposition [
	^classTraitComposition
]

{ #category : 'accessing' }
MCClassDefinition >> classTraitComposition: classTraitCompositionString [

	classTraitComposition := classTraitCompositionString
]

{ #category : 'accessing' }
MCClassDefinition >> classTraitCompositionCompiled [
		^(Smalltalk compiler evaluate: self classTraitCompositionString) asTraitComposition 
]

{ #category : 'accessing' }
MCClassDefinition >> classTraitCompositionString [
	^self classTraitComposition ifNil: ['{}'].
]

{ #category : 'accessing' }
MCClassDefinition >> classVarNames [
	^(self selectVariables: #isClassVariable) asArray sort
]

{ #category : 'accessing' }
MCClassDefinition >> classVarNames: cvarArray [

	self addVariables: cvarArray asSortedCollection ofType: MCClassVariableDefinition
]

{ #category : 'printing' }
MCClassDefinition >> classVariables [
	^ self needsSlotClassDefinition 
 		ifTrue: [ self sortedVariablesOfType: #isClassVariable ]
 		ifFalse: [ self classVariablesString asClassVariableCollection ]
]

{ #category : 'printing' }
MCClassDefinition >> classVariablesString [
	^ self stringForSortedVariablesOfType: #isClassVariable
]

{ #category : 'accessing' }
MCClassDefinition >> comment [
	^ comment
]

{ #category : 'accessing' }
MCClassDefinition >> comment: aString [

	comment := aString withInternalLineEndings
]

{ #category : 'accessing' }
MCClassDefinition >> commentStamp [
	^ commentStamp
]

{ #category : 'accessing' }
MCClassDefinition >> commentStamp: anObject [
	commentStamp := anObject
]

{ #category : 'installing' }
MCClassDefinition >> createClass [

	| superClass |
	"Ignore Context definition because of troubles with class migration on bootstrapped image. Temporary solution."
	name = #Context ifTrue: [
		Context comment = comment ifFalse: [ Context comment: comment stamp: self commentStamp ].
		^ self ].

	superClass := superclassName = #nil ifFalse: [ self class environment at: superclassName ].
	^ [
	  self class classInstaller make: [ :builder |
		  builder
			  superclass: superClass;
			  name: name;
			  layoutClass: (ObjectLayout layoutForType: type);
			  slots: self instanceVariables;
			  sharedVariables: self classVariables;
			  sharedPoolsFromString: self sharedPoolsString;
			  classSlots: self classInstanceVariables;
			  traitComposition: self traitCompositionCompiled;
			  classTraitComposition: self classTraitCompositionCompiled;
			  comment: comment stamp: self commentStamp;
			  package: self packageName;
			  tag: self tagName;
			  environment: superClass environment ] ]
		  on: Warning , DuplicatedVariableError
		  do: [ :ex | ex resume ]
]

{ #category : 'installing' }
MCClassDefinition >> createVariableFromString: aString [
	^[ Smalltalk compiler evaluate: aString ] 
		on: Error 
		do: [ 
			"if an error happens, we parse the slot definition to an ast.
			and create a UndefinedSlot"
			| ast slotName |
			ast := RBParser parseExpression: aString. 
			slotName := ast receiver value. 
			UndefinedSlot named: slotName ast: ast  ]
]

{ #category : 'accessing' }
MCClassDefinition >> defaultCommentStamp [
	^ String new

	"The version below avoids stomping on stamps already in the image

	^ (Smalltalk globals at: name ifPresent: [:c | c organization commentStamp])
		ifNil: ['']
	"

]

{ #category : 'printing' }
MCClassDefinition >> definitionString [
	^ String streamContents: [:stream | self printDefinitionOn: stream]
]

{ #category : 'accessing' }
MCClassDefinition >> description [
	^ { name }

]

{ #category : 'testing' }
MCClassDefinition >> hasClassInstanceVariables [
	^ (self selectVariables: #isClassInstanceVariable) isEmpty not
]

{ #category : 'testing' }
MCClassDefinition >> hasClassTraitComposition [
	^self classTraitCompositionString ~= '{}'
]

{ #category : 'testing' }
MCClassDefinition >> hasComment [
	^ comment isEmptyOrNil not
]

{ #category : 'testing' }
MCClassDefinition >> hasTraitComposition [
	^self traitCompositionString ~= '{}'
]

{ #category : 'comparing' }
MCClassDefinition >> hash [

	^ ((((((name hash bitXor: superclassName hash) bitXor: traitComposition hash) bitXor: classTraitComposition hash) bitXor: packageName hash) bitXor:
		    tagName hash) bitXor: type hash) bitXor: variables hash
]

{ #category : 'initialization' }
MCClassDefinition >> initialize [

	super initialize.
	superclassName := #Object.
	traitComposition := '{}'.
	classTraitComposition := '{}'.
	type := #normal.
	comment := ''.
	commentStamp := self defaultCommentStamp.
	variables := OrderedCollection new
]

{ #category : 'accessing' }
MCClassDefinition >> instVarNames [
	^ self selectVariables: #isInstanceVariable
]

{ #category : 'accessing' }
MCClassDefinition >> instVarNames: ivarArray [

	self addVariables: ivarArray ofType: MCInstanceVariableDefinition
]

{ #category : 'printing' }
MCClassDefinition >> instanceVariables [
	^self needsSlotClassDefinition 
		ifTrue: [self variablesOfType: #isInstanceVariable]
		ifFalse: [self instanceVariablesString asSlotCollection]
]

{ #category : 'printing' }
MCClassDefinition >> instanceVariablesString [
	^ self stringForVariablesOfType: #isInstanceVariable
]

{ #category : 'testing' }
MCClassDefinition >> isClassDefinition [
	^ true
]

{ #category : 'printing' }
MCClassDefinition >> kindOfSubclass [
	type = #normal ifTrue: [^ ' subclass: '].
	type = #words ifTrue: [^ ' variableWordSubclass: '].
	type = #variable ifTrue: [^ ' variableSubclass: '].
	type = #bytes ifTrue: [^ ' variableByteSubclass: '].
	type = #weak ifTrue: [^ ' weakSubclass: ' ].
	type = #compiledMethod ifTrue: [^ ' variableByteSubclass: ' ].
	type = #immediate ifTrue: [ ^ ' immediateSubclass: ' ].
	type = #ephemeron ifTrue: [ ^ ' ephemeronSubclass: ' ].
	type = #DoubleByteLayout ifTrue: [ ^' variableDoubleByteSubclass: ' ].
	type = #DoubleWordLayout ifTrue: [ ^' variableDoubleWordSubclass: ' ].
	"To support user defined layouts (load them as normal classes), we just return the default"
	^ ' subclass: '
]

{ #category : 'printing' }
MCClassDefinition >> layoutClassName [

	type = #normal ifTrue: [ ^ #FixedLayout ].
	type = #words ifTrue: [ ^ #WordLayout ].
	type = #variable ifTrue: [ ^ #VariableLayout ].
	type = #bytes ifTrue: [ ^ #ByteLayout ].
	type = #weak ifTrue: [ ^ #WeakLayout ].
	type = #compiledMethod ifTrue: [ ^ #CompiledMethodLayout ].
	type = #immediate ifTrue: [ ^ #ImmediateLayout ].
	type = #ephemeron ifTrue: [ ^ #EphemeronLayout ].
	type = #DoubleByteLayout ifTrue: [ ^ #DoubleByteLayout ].
	type = #DoubleWordLayout ifTrue: [ ^ #DoubleWordLayout ].
	"To support user defined layouts (load them as normal classes), we just return the default"
	^ #FixedLayout
]

{ #category : 'installing' }
MCClassDefinition >> load [
	self createClass
]

{ #category : 'accessing' }
MCClassDefinition >> name [
	^ name
]

{ #category : 'accessing' }
MCClassDefinition >> name: anObject [

	name := anObject asSymbol.
	self type: type
]

{ #category : 'installing' }
MCClassDefinition >> needsSlotClassDefinition [
	"this checks if any ivar or class var is using more than just standard definitions.
	Complex vars are encoded with a string that starts with a # or one that has a space"
	
	^self variables anySatisfy: [:var | (var name beginsWith:'#') or: [ var name includes: Character space ]]
]

{ #category : 'accessing' }
MCClassDefinition >> packageName [

	^ packageName
]

{ #category : 'accessing' }
MCClassDefinition >> packageName: aString [

	packageName := aString ifNotNil: [ :package | package asSymbol ]
]

{ #category : 'accessing' }
MCClassDefinition >> poolDictionaries [
	^ self selectVariables: #isPoolImport
]

{ #category : 'accessing' }
MCClassDefinition >> poolDictionaryNames: poolArray [

	self addVariables: poolArray asSortedCollection ofType: MCPoolImportDefinition
]

{ #category : 'copying' }
MCClassDefinition >> postCopy [

	super postCopy.
	variables := variables copy
]

{ #category : 'printing' }
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

{ #category : 'printing' }
MCClassDefinition >> printDefinitionBodyOn: stream [

	self hasClassTraitComposition ifTrue: [
		stream
			nextPutAll: 'uses: ';
			nextPutAll: self classTraitCompositionString;
			cr; tab ].
	stream
		nextPutAll: ' instanceVariableNames: '; 
		store: self classInstanceVariablesString
]

{ #category : 'printing' }
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

{ #category : 'printing' }
MCClassDefinition >> printMetaDefinitionOn: stream [
	stream 
		nextPutAll: self className;
		nextPutAll: ' class'; cr; tab.
	self printDefinitionBodyOn: stream
]

{ #category : 'comparing' }
MCClassDefinition >> provisions [
	^ { name }
]

{ #category : 'comparing' }
MCClassDefinition >> requirements [

	^ (superclassName = #nil or: [ superclassName asString beginsWith: 'AnObsolete' ])
		  ifTrue: [ self poolDictionaries ]
		  ifFalse: [ { superclassName } , self poolDictionaries ]
]

{ #category : 'accessing' }
MCClassDefinition >> selectVariables: aSelector [
	^ variables select: [:v | v perform: aSelector] thenCollect: [:v | v name]
]

{ #category : 'printing' }
MCClassDefinition >> sharedPoolsString [
	^ self stringForSortedVariablesOfType: #isPoolImport
]

{ #category : 'accessing' }
MCClassDefinition >> sortKey [
	^ self className
]

{ #category : 'accessing' }
MCClassDefinition >> sortedVariables [
	"sort variables for comparison purposes"

	| sorted |
	sorted := variables select: [:var | var isOrderDependend].
	sorted addAll: ((variables reject: [:var | var isOrderDependend])
		asSortedCollection: [:a :b | a name <= b name]).
	^sorted
]

{ #category : 'installing' }
MCClassDefinition >> sortedVariablesOfType: aSymbol [
	"version for complex vars, { definition . definition }"
	^(((self selectVariables: aSymbol) asSortedCollection collect: [:each | Smalltalk compiler evaluate: each])) asArray.
		
]

{ #category : 'printing' }
MCClassDefinition >> source [
	^ self definitionString
]

{ #category : 'installing' }
MCClassDefinition >> stringForSortedVariablesOfType: aSymbol [
	^ String streamContents:
		[:stream |
		(self selectVariables: aSymbol) asSortedCollection
			do: [:ea | stream nextPutAll: ea]
			separatedBy: [stream space]]
]

{ #category : 'installing' }
MCClassDefinition >> stringForVariablesOfType: aSymbol [
	^ String streamContents:
		[:stream |
		(self selectVariables: aSymbol) 
			do: [:ea | stream nextPutAll: ea]
			separatedBy: [stream space]]
]

{ #category : 'printing' }
MCClassDefinition >> summary [
	^ name
]

{ #category : 'accessing' }
MCClassDefinition >> superclass [
	
	^ superclassName
]

{ #category : 'accessing' }
MCClassDefinition >> superclassName [
	^ superclassName
]

{ #category : 'accessing' }
MCClassDefinition >> superclassName: superclassString [

	superclassName := superclassString
		                  ifNil: [ #nil ]
		                  ifNotNil: [ superclassString asSymbol ]
]

{ #category : 'accessing' }
MCClassDefinition >> tagName [

	^ tagName
]

{ #category : 'accessing' }
MCClassDefinition >> tagName: aString [

	tagName := aString = Package rootTagName
		           ifTrue: [ nil ]
		           ifFalse: [ aString ifNotNil: [ aString asSymbol ] ]
]

{ #category : 'accessing' }
MCClassDefinition >> traitComposition [
	^traitComposition
]

{ #category : 'accessing' }
MCClassDefinition >> traitComposition: traitCompositionString [

	traitComposition := traitCompositionString
]

{ #category : 'accessing' }
MCClassDefinition >> traitCompositionCompiled [
	^(Smalltalk compiler evaluate: self traitCompositionString) asTraitComposition 
]

{ #category : 'accessing' }
MCClassDefinition >> traitCompositionString [
	^self traitComposition ifNil: ['{}'].
]

{ #category : 'accessing' }
MCClassDefinition >> type [
	^ type
]

{ #category : 'accessing' }
MCClassDefinition >> type: aSymbol [

	type := (#( #CompiledMethod #CompiledBlock #CompiledCode ) includes: name)
		        ifTrue: [ #compiledMethod ]
		        ifFalse: [ aSymbol ]
]

{ #category : 'installing' }
MCClassDefinition >> unload [

	self class environment removeClassNamed: name
]

{ #category : 'accessing' }
MCClassDefinition >> variables [
	^ variables ifNil: [ variables := OrderedCollection  new ]
]

{ #category : 'installing' }
MCClassDefinition >> variablesOfType: aSymbol [
	"version for complex vars, { definition . definition }"
	^(self selectVariables: aSymbol) 
		collect: [:each | self createVariableFromString: each]
		as: Array
]
