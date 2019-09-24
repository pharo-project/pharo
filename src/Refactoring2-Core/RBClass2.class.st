"
I represent the instance side of a class in a refactoring namespace. 

You can query methods and (instance) var names, poolDictionaries, class category and comment.

I shouldn't be created directly, but always be part of a refactoring namespace.
My  namespace usally knows me and my meta class.
"
Class {
	#name : #RBClass2,
	#superclass : #RBAbstractClass2,
	#instVars : [
		'classVariableNames',
		'poolDictionaryNames',
		'category',
		'comment'
	],
	#classVars : [
		'LookupComment'
	],
	#category : #'Refactoring2-Core-Model'
}

{ #category : #'class initialization' }
RBClass2 class >> initialize [

	LookupComment := Object new
]

{ #category : #transforming }
RBClass2 >> addClassVariable: aString [ 

	self privateClassVariableNames add: aString asSymbol.
	model addClassVariable: aString to: self
]

{ #category : #transforming }
RBClass2 >> addProtocolNamed: aString [

	model addProtocolNamed: aString in: self
]

{ #category : #accessing }
RBClass2 >> category [

	^ category ifNil: [ 
		self isDefined
			ifTrue: [category := self realClass category]
			ifFalse: [model environment whichCategoryIncludes: self name]]

]

{ #category : #accessing }
RBClass2 >> category: anObject [
	category := anObject
]

{ #category : #accessing }
RBClass2 >> classVariableNames [
	
	^ self privateClassVariableNames copy
]

{ #category : #accessing }
RBClass2 >> classVariableNames: aCollectionOfStrings [ 

	classVariableNames := (aCollectionOfStrings collect: [:each | each asSymbol]) asOrderedCollection
]

{ #category : #accessing }
RBClass2 >> comment [

	^ comment = LookupComment
		ifTrue: [
			comment := self isDefined
				ifTrue: [ self realClass comment ]
				ifFalse: [ nil ] ]
		ifFalse: [ comment ]
]

{ #category : #accessing }
RBClass2 >> comment: aString [

	self model changes comment: (comment := aString) in: self
]

{ #category : #accessing }
RBClass2 >> definitionString [

	^ String streamContents: [ :definitionStream | 
		definitionStream
			nextPutAll: self superclass printString;
			nextPutAll: ' subclass: #';
			nextPutAll: self name;
			nextPutAll:	' instanceVariableNames: '''.
				self instanceVariableNames do: [ :each | 
					definitionStream nextPutAll: each; nextPut: $  ].
		definitionStream
			nextPutAll: ''' classVariableNames: '''.
				self classVariableNames do: [ :each | 
					definitionStream nextPutAll: each; nextPut: $  ].
		definitionStream
			nextPutAll: ''' poolDictionaries: '''.
				self poolDictionaryNames do: [ :each | 
					definitionStream nextPutAll: each; nextPut: $  ].
		definitionStream
			nextPutAll: ''' category: #'''.
		definitionStream nextPutAll: self category asString.
		definitionStream nextPut: $' ]
]

{ #category : #querying }
RBClass2 >> directlyDefinesClassVariable: aString [

	^ self classVariableNames includes: aString asSymbol
]

{ #category : #testing }
RBClass2 >> isMeta [

	^ false
]

{ #category : #accessing }
RBClass2 >> poolDictionaryNames [

	^ self privatePoolDictionaryNames copy
]

{ #category : #accessing }
RBClass2 >> poolDictionaryNames: aCollectionOfStrings [ 

	poolDictionaryNames := (aCollectionOfStrings collect: [:each | each asSymbol]) asOrderedCollection
]

{ #category : #private }
RBClass2 >> privateClassVariableNames [

	(self isDefined and: [ classVariableNames isNil ]) 
		ifTrue: [ self classVariableNames: self realClass classVarNames ].
	^ classVariableNames
]

{ #category : #private }
RBClass2 >> privatePoolDictionaryNames [

	(self isDefined and: [ poolDictionaryNames isNil ]) 
		ifTrue: [ self poolDictionaryNames:
			(self realClass sharedPools collect: [ :each | self realClass environment keyAtValue: each ])].
	^ poolDictionaryNames
]

{ #category : #accessing }
RBClass2 >> realName: aSymbol [

	self realClass: (self class environment at: aSymbol)
]

{ #category : #transforming }
RBClass2 >> removeClassVariable: aString [ 

	self privateClassVariableNames remove: aString asSymbol.
	model removeClassVariable: aString from: self
]

{ #category : #transforming }
RBClass2 >> removeProtocolNamed: aString [

	model removeProtocolNamed: aString in: self
]

{ #category : #transforming }
RBClass2 >> renameClassVariable: oldName to: newName around: aBlock [ 

	self privateClassVariableNames 
		at: (self privateClassVariableNames indexOf: oldName asSymbol)
		put: newName asSymbol.
		
	^ self model
		performChange: (changeFactory  
								renameClassVariable: oldName
								to: newName
								in: self)
		around: aBlock
]

{ #category : #accessing }
RBClass2 >> sharedPoolNames [

	^ self privatePoolDictionaryNames copy
]

{ #category : #accessing }
RBClass2 >> theNonMetaClass [

	^ self
]
