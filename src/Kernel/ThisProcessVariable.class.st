"
I model thisProcess keyword
"
Class {
	#name : #ThisProcessVariable,
	#superclass : #ReservedVariable,
	#category : #'Kernel-Variables'
}

{ #category : #accessing }
ThisProcessVariable class >> variableName [
	^#thisProcess
]

{ #category : #'code generation' }
ThisProcessVariable >> emitValue: methodBuilder [

	methodBuilder pushThisProcess 
]

{ #category : #testing }
ThisProcessVariable >> isThisProcessVariable [
	^true
]

{ #category : #debugging }
ThisProcessVariable >> readInContext: aContext [

	^ aContext findProcess 
]
