"
I represent the class side of a class in a refactoring namespace. 

I shouldn't be created directly, but always be part of a refactoring namespace.
My  namespace usally knows me and my non meta class.
"
Class {
	#name : #RBMetaclass,
	#superclass : #RBAbstractClass,
	#category : #'Refactoring-Core-Model'
}

{ #category : #'instance creation' }
RBMetaclass class >> existingNamed: aSymbol [ 
	^(self named: aSymbol)
		realName: aSymbol;
		yourself
]

{ #category : #'instance creation' }
RBMetaclass class >> named: aSymbol [ 
	^(self new)
		name: aSymbol;
		yourself
]

{ #category : #accessing }
RBMetaclass >> allClassVariableNames [
	^ self instanceSide allClassVariableNames
]

{ #category : #accessing }
RBMetaclass >> allPoolDictionaryNames [
	^ self instanceSide allPoolDictionaryNames
]

{ #category : #accessing }
RBMetaclass >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test." 
	
	^ self
]

{ #category : #testing }
RBMetaclass >> directlyDefinesClassVariable: aString [ 
	^ self instanceSide directlyDefinesClassVariable: aString
]

{ #category : #testing }
RBMetaclass >> directlyDefinesPoolDictionary: aString [ 
	^ self instanceSide directlyDefinesPoolDictionary: aString
]

{ #category : #testing }
RBMetaclass >> isMeta [
	^true
]

{ #category : #printing }
RBMetaclass >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' class'
]

{ #category : #initialization }
RBMetaclass >> realName: aSymbol [ 
	self realClass: (Smalltalk globals at: aSymbol) classSide
]

{ #category : #printing }
RBMetaclass >> storeOn: aStream [ 
	super storeOn: aStream.
	aStream nextPutAll: ' class'
]

{ #category : #'accessing - deprecated' }
RBMetaclass >> theMetaClass [
	^ self
]
