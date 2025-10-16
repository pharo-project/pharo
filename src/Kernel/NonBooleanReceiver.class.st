"
Some constructs are optimized in the compiler :
#whileTrue:
#whileFalse:
#ifTrue:
#ifFalse:
#ifTrue:ifFalse:
#ifFalse:ifTrue:
So you cannot by default use them on non boolean objects.
	
If you really need to use optimized constructs, you can enable Opal compiler and do one of the following :
		- recompile your method with the pragma : <compilerOptions: #(+ optIlineNone)>
		- recompile your class with the method : MyClass class>>compiler 
			^ super compiler options: #(+ optIlineNone)
		- call from this method by Object>>#mustBeBooleanInMagic:""
"
Class {
	#name : #NonBooleanReceiver,
	#superclass : #Error,
	#instVars : [
		'object'
	],
	#classVars : [
		'MustBeBooleanHandler'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #'must-be-boolean-mop' }
NonBooleanReceiver class >> mustBeBooleanHandler [

	^ MustBeBooleanHandler
]

{ #category : #'must-be-boolean-mop' }
NonBooleanReceiver class >> mustBeBooleanHandler: aHandler [

	MustBeBooleanHandler := aHandler
]

{ #category : #testing }
NonBooleanReceiver >> isResumable [

	^ true
]

{ #category : #accessing }
NonBooleanReceiver >> object [
	^object
]

{ #category : #accessing }
NonBooleanReceiver >> object: anObject [
	object := anObject
]
