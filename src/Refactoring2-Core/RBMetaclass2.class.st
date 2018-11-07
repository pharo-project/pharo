"
I represent the class side of a class in a refactoring namespace. 

I shouldn't be created directly, but always be part of a refactoring namespace.
My  namespace usally knows me and my non meta class.
"
Class {
	#name : #RBMetaclass2,
	#superclass : #RBAbstractClass2,
	#category : #'Refactoring2-Core-Model'
}

{ #category : #querying }
RBMetaclass2 >> directlyDefinesClassVariable: aString [

	^ self theNonMetaClass directlyDefinesClassVariable: aString
]

{ #category : #testing }
RBMetaclass2 >> isMeta [

	^ true
]

{ #category : #accessing }
RBMetaclass2 >> realName: aSymbol [

	self realClass: (Smalltalk globals at: aSymbol) classSide
]
