"
This error is signaled on write attempt to undeclared variables
"
Class {
	#name : #UndeclaredVariableWrite,
	#superclass : #Error,
	#instVars : [
		'variable',
		'value'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #testing }
UndeclaredVariableWrite >> defaultResumeValue [

	^ value
]

{ #category : #testing }
UndeclaredVariableWrite >> isResumable [
	"The undeclared variable write become a no-op"

	^ true
]

{ #category : #accessing }
UndeclaredVariableWrite >> value [

	^ value
]

{ #category : #accessing }
UndeclaredVariableWrite >> value: anObject [

	value := anObject
]

{ #category : #accessing }
UndeclaredVariableWrite >> variable [

	^ variable
]

{ #category : #accessing }
UndeclaredVariableWrite >> variable: anObject [

	variable := anObject
]
