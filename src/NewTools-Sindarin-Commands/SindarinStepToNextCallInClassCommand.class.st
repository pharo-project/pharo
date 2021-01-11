"
I will step the execution from a context until the next call to the same class as that context's receiver's class.
"
Class {
	#name : #SindarinStepToNextCallInClassCommand,
	#superclass : #SindarinStepToNextCallCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #default }
SindarinStepToNextCallInClassCommand class >> defaultDescription [
	^ 'Steps execution until the next call in the current class, then returns debugger control.'
]

{ #category : #default }
SindarinStepToNextCallInClassCommand class >> defaultName [
	<toolbarExtensionDebugCommand: 50>
	<codeExtensionDebugCommand: 50>
	^ 'Next call in class'
]

{ #category : #predicates }
SindarinStepToNextCallInClassCommand >> setConditionValue [

	conditionValue := debugger receiver class
]

{ #category : #predicates }
SindarinStepToNextCallInClassCommand >> targetCondition [ 
	^debugger receiver class ~= conditionValue
]
