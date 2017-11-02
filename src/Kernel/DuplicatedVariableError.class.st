"
I am an error signalled when a variable is redeclared. For instance when a method is created wich has a temporary or argument with the same name as an instance variable.
"
Class {
	#name : #DuplicatedVariableError,
	#superclass : #Error,
	#instVars : [
		'superclass',
		'variable'
	],
	#category : 'Kernel-Exceptions'
}

{ #category : #testing }
DuplicatedVariableError >> isResumable [
        ^true
]

{ #category : #accessing }
DuplicatedVariableError >> superclass [
        "The superclass in which the variable is defined"
        ^superclass
]

{ #category : #accessing }
DuplicatedVariableError >> superclass: aSuperclass [
        "The superclass in which the variable is defined"
        superclass := aSuperclass
]

{ #category : #accessing }
DuplicatedVariableError >> variable [
        "Name of the duplicate variable"
        ^variable
]

{ #category : #accessing }
DuplicatedVariableError >> variable: aVariable [
        "Name of the duplicate variable"
        variable := aVariable
]
