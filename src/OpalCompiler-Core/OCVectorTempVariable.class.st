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

{ #category : #debugging }
OCVectorTempVariable >> indexFromIR [
	^scope methodScope node irInstruction indexForVarNamed: name
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

{ #category : #debugging }
OCVectorTempVariable >> searchFromContext: aContext scope: contextScope [

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

{ #category : #debugging }
OCVectorTempVariable >> writeFromContext: aContext scope: contextScope value: aValue [
	| tempVectorVar theVector offset |
	tempVectorVar := contextScope lookupVar: vectorName.
	theVector := tempVectorVar readFromContext: aContext scope: tempVectorVar scope.
	offset := tempVectorVar indexInTempVectorFromIR: name.
	^theVector at: offset put: aValue.
]
