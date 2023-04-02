"
This error is signaled on write attempt to undeclared variables
"
Class {
	#name : #UndeclaredVariableWrite,
	#superclass : #Error,
	#instVars : [
		'variable'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #testing }
UndeclaredVariableWrite >> isResumable [
	"The undeclared variable write become a no-op"

	^ true
]

{ #category : #accessing }
UndeclaredVariableWrite >> variable [

	^ variable
]

{ #category : #accessing }
UndeclaredVariableWrite >> variable: anObject [

	variable := anObject
]
