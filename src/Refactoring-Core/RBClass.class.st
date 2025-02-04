"
I represent the instance side of a class in a refactoring namespace. 

You can query methods and (instance) var names, poolDictionaries, class category and comment.

I shouldn't be created directly, but always be part of a refactoring namespace.
My  namespace usally knows me and my meta class.
"
Class {
	#name : 'RBClass',
	#superclass : 'RBAbstractClass',
	#instVars : [
		'classVariableNames',
		'poolDictionaryNames',
		'comment',
		'classPool',
		'packageName',
		'tagName',
		'traitComposition'
	],
	#classVars : [
		'LookupComment'
	],
	#category : 'Refactoring-Core-Model',
	#package : 'Refactoring-Core',
	#tag : 'Model'
}

{ #category : 'instance creation' }
RBClass class >> existingNamed: aSymbol [
	^(self named: aSymbol)
		realName: aSymbol;
		yourself
]

{ #category : 'instance creation' }
RBClass class >> existingNamed: aSymbol model: aRBNamespace [
	^ (self named: aSymbol)
		model: aRBNamespace;
		realName: aSymbol;
		yourself
]

{ #category : 'class initialization' }
RBClass class >> initialize [
	LookupComment := Object new
]

{ #category : 'instance creation' }
RBClass class >> named: aSymbol [
	^ self new 
		name: aSymbol;
		yourself
]

{ #category : 'variable accessing' }
RBClass >> addClassVariable: aString [
	
	self privateClassVariableNames add: aString asSymbol.
	model addClassVariable: aString to: self.
	self classPool at: aString put: nil
]

{ #category : 'variable accessing' }
RBClass >> addPoolDictionary: aString [

	self privatePoolDictionaryNames add: aString asSymbol.
	model addPool: aString to: self
]

{ #category : 'adding' }
RBClass >> addProtocolNamed: aString [

	model addProtocolNamed: aString in: self
]

{ #category : 'accessing' }
RBClass >> allClassVariableNames [
	"Returns all the shared variables of the class (e.g. including its superclass)"
	
	| sprClass |
	sprClass := self superclass.
	^ sprClass
		ifNil: [ self classVariableNames ]
		ifNotNil: [ sprClass allClassVariableNames , self classVariableNames ]
]

{ #category : 'accessing' }
RBClass >> allPoolDictionaryNames [

	| sprClass |
	sprClass := self superclass.
	^ sprClass
		ifNil: [ self sharedPoolNames ]
		ifNotNil: [ sprClass allPoolDictionaryNames , self sharedPoolNames ]
]

{ #category : 'accessing' }
RBClass >> classPool [

	^ classPool ifNil: [ classPool := Dictionary new ]
]

{ #category : 'accessing' }
RBClass >> classVariableNames [
	"Returns a copy of the shared variable names of the receiver"
	
	^ self privateClassVariableNames copy
]

{ #category : 'accessing' }
RBClass >> classVariableNames: aCollectionOfStrings [
	"Initializing to nil the shared variables named in the argument."

	classVariableNames := (aCollectionOfStrings
				collect: [:each | self classPool at: each asSymbol put: nil.
					each asSymbol]) asOrderedCollection
]

{ #category : 'accessing' }
RBClass >> comment [
	"Compute if necessary the comment, store it and return it"
	
	^ comment = LookupComment
		ifTrue: [
			comment := self isDefined
				ifTrue: [ self realClass comment ]
				ifFalse: [ nil ] ]
		ifFalse: [ comment ]
]

{ #category : 'accessing' }
RBClass >> comment: aString [
	self model changes comment: (comment := aString) in: self
]

{ #category : 'testing' }
RBClass >> directlyDefinesClassVariable: aString [
	"Returns whether the class defines locally the shared variable whose name is given in argument."
	
	^ self classVariableNames includes: aString asSymbol
]

{ #category : 'testing' }
RBClass >> directlyDefinesPoolDictionary: aString [
	^self sharedPoolNames includes: aString asSymbol
]

{ #category : 'testing' }
RBClass >> hasSubclasses [

	^ self subclasses isNotEmpty
]

{ #category : 'initialization' }
RBClass >> initialize [
	"LookupComment is a tag indicating that the comment has not been set."
	
	super initialize.
	comment := LookupComment
]

{ #category : 'accessing' }
RBClass >> instanceSide [

	^ self
]

{ #category : 'testing' }
RBClass >> isEmptyClass [
	"A class is empty when it has no instance variable, shared variables and methods."
	
	^ self classVariableNames isEmpty and: [
				  self instanceVariableNames isEmpty and: [
					  self selectors isEmpty]]
]

{ #category : 'testing' }
RBClass >> isManifest [ 

	^ self superclass notNil and: [ self superclass name = #PackageManifest ]

]

{ #category : 'testing' }
RBClass >> isMeta [

	^false
]

{ #category : 'self evaluating' }
RBClass >> isSelfEvaluating [

	^ true
]

{ #category : 'testing' }
RBClass >> isSharedPool [

	^ (self allSuperclasses collect:  [:each | each name]) includes: #SharedPool
]

{ #category : 'accessing' }
RBClass >> methods [

	^ self allMethods asOrderedCollection 
]

{ #category : 'querying' }
RBClass >> methodsUsingClassVariableNamed: aClassVariableName [

	^ (self realClass classVariableNamed: aClassVariableName) usingMethods collect: [ :aMethod |
			|modelClass|
			modelClass := self model classNamed: aMethod methodClass name.
			modelClass methodFor: aMethod selector
		]
]

{ #category : 'accessing' }
RBClass >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RBClass >> packageName: anObject [

	packageName := anObject
]

{ #category : 'accessing' }
RBClass >> poolDictionaryNames [

	^ self privatePoolDictionaryNames copy
]

{ #category : 'accessing' }
RBClass >> poolDictionaryNames: aCollectionOfStrings [
	poolDictionaryNames := (aCollectionOfStrings collect: [:each | each asSymbol]) asOrderedCollection
]

{ #category : 'private' }
RBClass >> privateClassVariableNames [
	(self isDefined and: [classVariableNames isNil])
		ifTrue: [self classVariableNames: self realClass classVarNames].
	^classVariableNames
]

{ #category : 'private' }
RBClass >> privatePoolDictionaryNames [
	(self isDefined and: [poolDictionaryNames isNil])
		ifTrue:
			[self poolDictionaryNames: (self realClass sharedPools
						collect: [:each | self realClass environment keyAtValue: each])].
	^poolDictionaryNames
]

{ #category : 'private' }
RBClass >> realName: aSymbol [
	self realClass: (self model environment at: aSymbol)
]

{ #category : 'variable accessing' }
RBClass >> removeClassVariable: aString [
	self privateClassVariableNames remove: aString asSymbol.
	model removeClassVariable: aString from: self
]

{ #category : 'variable accessing' }
RBClass >> removeClassVariable: aString ifAbsent: aBlock [
	self privateClassVariableNames remove: aString asSymbol ifAbsent: aBlock.
	model removeClassVariable: aString from: self
]

{ #category : 'variable accessing' }
RBClass >> removePoolDictionary: aString [
	self privatePoolDictionaryNames remove: aString asSymbol
]

{ #category : 'removing' }
RBClass >> removeProtocolNamed: aString [

	model removeProtocolNamed: aString in: self
]

{ #category : 'variable accessing' }
RBClass >> renameClassVariable: oldName to: newName around: aBlock [

	self privateClassVariableNames
		at: (self privateClassVariableNames indexOf: oldName asSymbol)
		put: newName asSymbol.
	^ self model
		  perform: aBlock
		  onlyWithChange:
		  (changeFactory renameClassVariable: oldName to: newName in: self)
]

{ #category : 'accessing' }
RBClass >> sharedPoolNames [

	^self privatePoolDictionaryNames copy
]

{ #category : 'accessing' }
RBClass >> sharedPools [
	^ self allPoolDictionaryNames collect: [ :each | Smalltalk globals at: each asSymbol ifAbsent: [ Dictionary new ] ]
]

{ #category : 'accessing' }
RBClass >> tagName [

	^ tagName
]

{ #category : 'accessing' }
RBClass >> tagName: anObject [

	tagName := anObject
]

{ #category : 'accessing' }
RBClass >> traitComposition [
	
	^ traitComposition
]

{ #category : 'accessing' }
RBClass >> traitComposition: aComposition [
	
	traitComposition := aComposition
]
