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

{ #category : #accessing }
UndeclaredVariableRead >> variable [

	^ variable
]

{ #category : #accessing }
UndeclaredVariableRead >> variable: anObject [

	variable := anObject
]
