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
	"We expect this method to be called either from the same process or by debugger process that will read the correct process. Otherwise we cannot guarantee to read the correct process"
	^ Processor activeProcess
]
