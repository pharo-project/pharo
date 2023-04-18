"
I am an abstract superclass for exceptions related to a read and write access on undeclared variable.
"
Class {
	#name : #UndeclaredVariableError,
	#superclass : #Error,
	#instVars : [
		'variable'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #testing }
UndeclaredVariableError >> isResumable [
	"The undeclared variable write become a no-op"

	^ true
]

{ #category : #accessing }
UndeclaredVariableError >> variable [

	^ variable
]

{ #category : #accessing }
UndeclaredVariableError >> variable: anObject [

	variable := anObject
]
