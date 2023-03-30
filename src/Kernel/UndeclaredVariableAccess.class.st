"
Undeclared variable access is signaled on attempt to read an undeclared variable or store into one.
"
Class {
	#name : #UndeclaredVariableAccess,
	#superclass : #Error,
	#instVars : [
		'variable'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #accessing }
UndeclaredVariableAccess >> variable [

	^ variable
]

{ #category : #accessing }
UndeclaredVariableAccess >> variable: anObject [

	variable := anObject
]
