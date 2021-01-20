"
I will step the execution from a context until the next call to the same object as that context's receiver.
"
Class {
	#name : #SindarinStepToNextCallInObjectCommand,
	#superclass : #SindarinStepToNextCallCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #default }
SindarinStepToNextCallInObjectCommand class >> defaultDescription [
	^ 'Steps execution until the next call in the current receiver, then returns debugger control.'
]

{ #category : #default }
SindarinStepToNextCallInObjectCommand class >> defaultName [
	<toolbarExtensionDebugCommand: 50>
	<codeExtensionDebugCommand: 50>
	^ 'Next call in receiver'
]

{ #category : #predicates }
SindarinStepToNextCallInObjectCommand >> setConditionValue [

	conditionValue := debugger receiver
]

{ #category : #predicates }
SindarinStepToNextCallInObjectCommand >> targetCondition [ 
	^debugger receiver ~= conditionValue
]
