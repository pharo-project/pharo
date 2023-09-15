"
I model thisProcess
"
Class {
	#name : #ThisProcessVariable,
	#superclass : #PseudoVariable,
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
