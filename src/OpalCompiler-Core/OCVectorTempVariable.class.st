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
	"We need to first read the temp vector. We use the closest copied var up the stack possible as
	 as the definition context could be already garbage collected"
	tempVectorVar := contextScope lookupVar: vectorName.
	"We might be called from the debugger for a context that actually does not access the temp vector, 
	so we need to read possibly from a context above aContext"
	theVector := tempVectorVar readFromContext: aContext scope: contextScope.
	"Cache the index in the temp vecor (for speed and easy debugging)"
	index := index ifNil: [ tempVectorVar originalVar indexInTempVectorFromIR: name ].
	"We can call this method on a context in any state, e.g. even when the copied var is not yet initialized.
	In this case, we lookup in the outer context with the corresponding outer scope"
	^ theVector ifNil: [ aContext outerContext 
		"but there might be no outer context, so in that case, just return an empty array of the correct size"
			ifNil: [ Array new: tempVectorVar originalVar scope tempVector size ]
			ifNotNil: [: ctxt | self readVectorFromContext: ctxt scope: contextScope outerScope ]]
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
	^index ifNil: [index := (scope lookupVar: vectorName) originalVar indexInTempVectorFromIR: name]
]

{ #category : #debugging }
OCVectorTempVariable >> writeFromContext: aContext scope: contextScope value: aValue [

	| theVector |
	theVector := self readVectorFromContext: aContext scope: contextScope.
	^theVector at: index put: aValue.
]
