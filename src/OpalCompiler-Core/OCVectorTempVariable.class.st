"
I model a temp variable that is stored in a temp vector.
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

	| offset pairVariableVector |
	pairVariableVector := contextScope lookupTempVector: vectorName inContext: aContext.
	offset := pairVariableVector first indexInTempVectorFromIR: name.
	^pairVariableVector second at: offset.
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
	^(scope lookupVar: vectorName) indexInTempVectorFromIR: name
]

{ #category : #debugging }
OCVectorTempVariable >> writeFromContext: aContext scope: contextScope value: aValue [
	| tempVectorVar theVector offset |
	tempVectorVar := contextScope lookupVar: vectorName.
	theVector := tempVectorVar readFromLocalContext: aContext.
	offset := tempVectorVar indexInTempVectorFromIR: name.
	^theVector at: offset put: aValue.
]
