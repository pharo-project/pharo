"
This error is signaled on read attempt of undeclared variables
"
Class {
	#name : #UndeclaredVariableRead,
	#superclass : #Error,
	#instVars : [
		'variable'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #testing }
UndeclaredVariableRead >> isResumable [
	"The undeclared variable read give the resumed value (nil by default)"

	^ true
]

{ #category : #accessing }
UndeclaredVariableRead >> variable [

	^ variable
]

{ #category : #accessing }
UndeclaredVariableRead >> variable: anObject [

	variable := anObject
]
