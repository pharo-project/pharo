"
I validate a Clap context by checking that it has matched completely, i.e. no word remains unmatched by a parameter.
"
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
