"
I model a temp variable that is stored in a temp vector.

My ""index"" variable has the index where I am stored in the temp vector (filled in lazily if requested, the index is determined by the bytecode backend and thus the result of the compilation)

The temp vector itself is stored in a copied var (you can get it's name by sending #vectorName) 

reading and writing thus is a multi step process:

1) get the semamtic variable that stores the temp vector by name
2) get the offset in the temp vector from it (cached in ""index"")
3) the temp vector semantic var knows how to read / write the vector (which is an array)
4) use the index to access this array
"
Class {
	#name : #OCVectorTempVariable,
	#superclass : #OCTempVariable,
	#instVars : [
		'vectorName'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #emitting }
OCVectorTempVariable >> emitStore: methodBuilder [

	methodBuilder storeRemoteTemp: name inVector: vectorName.
]

{ #category : #emitting }
OCVectorTempVariable >> emitValue: methodBuilder [

	methodBuilder pushRemoteTemp: name inVector: vectorName.
]

{ #category : #testing }
OCVectorTempVariable >> isRemote [
	^true
]

{ #category : #testing }
OCVectorTempVariable >> isTempVectorTemp [
	^true
]

{ #category : #debugging }
OCVectorTempVariable >> readFromContext: aContext scope: contextScope [

	| theVector |
	theVector := self readVectorFromContext: aContext scope: contextScope.
	^theVector at: index
]

{ #category : #debugging }
OCVectorTempVariable >> readVectorFromContext: aContext scope: contextScope [
	| tempVectorVar theVector |
	tempVectorVar := contextScope lookupVar: vectorName.
	theVector := tempVectorVar readFromLocalContext: aContext.
	index := index ifNil: [ tempVectorVar indexInTempVectorFromIR: name ].

	"If we don't find the temp vector (because it was nilled or not created), we 
	 lookup in the outer context with the corresponding outer scope"
	^ theVector ifNil: [ self readVectorFromContext: aContext outerContext scope: contextScope outerScope ]
]

{ #category : #accessing }
OCVectorTempVariable >> vectorName [
	^ vectorName
]

{ #category : #accessing }
OCVectorTempVariable >> vectorName: anObject [
	vectorName := anObject
]

{ #category : #accessing }
OCVectorTempVariable >> vectorOffset [
	"Temps that are stored in a temp vector have a unique index in the vector.
	 We first lookup the temp vector sem var by name and then get the index from the IR"
	^index ifNil: [index := (scope lookupVar: vectorName) indexInTempVectorFromIR: name]
]

{ #category : #debugging }
OCVectorTempVariable >> writeFromContext: aContext scope: contextScope value: aValue [

	| theVector |
	theVector := self readVectorFromContext: aContext scope: contextScope.
	^theVector at: index put: aValue.
]
