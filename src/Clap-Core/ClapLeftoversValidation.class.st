Class {
	#name : #ClapLeftoversValidation,
	#superclass : #ClapValidation,
	#category : #'Clap-Core-Validation'
}

{ #category : #validation }
ClapLeftoversValidation >> description [
	^ 'Unrecognized arguments'
]

{ #category : #validation }
ClapLeftoversValidation >> matches: aClapExpression [
	^ aClapExpression context leftovers isEmpty
]
