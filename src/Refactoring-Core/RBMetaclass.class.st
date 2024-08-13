"
I represent the class side of a class in a refactoring namespace. 

I shouldn't be created directly, but always be part of a refactoring namespace.
My  namespace usally knows me and my non meta class.
"
Class {
	#name : 'RBMetaclass',
	#superclass : 'RBAbstractClass',
	#category : 'Refactoring-Core-Model',
	#package : 'Refactoring-Core',
	#tag : 'Model'
}

{ #category : 'instance creation' }
RBMetaclass class >> existingNamed: aSymbol [
	^(self named: aSymbol)
		realName: aSymbol;
		yourself
]

{ #category : 'instance creation' }
RBMetaclass class >> existingNamed: aSymbol model: aRBNamespace [
	^ (self named: aSymbol)
		model: aRBNamespace;
		realName: aSymbol;
		yourself
]

{ #category : 'instance creation' }
RBMetaclass class >> named: aSymbol [
	^(self new)
		name: aSymbol;
		yourself
]

{ #category : 'accessing' }
RBMetaclass >> allClassVariableNames [
	^ self instanceSide allClassVariableNames
]

{ #category : 'accessing' }
RBMetaclass >> allPoolDictionaryNames [
	^ self instanceSide allPoolDictionaryNames
]

{ #category : 'accessing' }
RBMetaclass >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test."

	^ self
]

{ #category : 'testing' }
RBMetaclass >> directlyDefinesClassVariable: aString [

	^ self instanceSide directlyDefinesClassVariable: aString
]

{ #category : 'testing' }
RBMetaclass >> directlyDefinesPoolDictionary: aString [
	^ self instanceSide directlyDefinesPoolDictionary: aString
]

{ #category : 'testing' }
RBMetaclass >> isManifest [

	^ self instanceSide isManifest
]

{ #category : 'testing' }
RBMetaclass >> isMeta [
	^true
]

{ #category : 'printing' }
RBMetaclass >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' class'
]

{ #category : 'accessing' }
RBMetaclass >> realName: aSymbol [
	self realClass: (self model environment at: aSymbol) classSide
]

{ #category : 'storing' }
RBMetaclass >> storeOn: aStream [
	super storeOn: aStream.
	aStream nextPutAll: ' class'
]
