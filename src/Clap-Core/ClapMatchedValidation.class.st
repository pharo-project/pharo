"
I validate a Clap context by checking that it has matched.
"
Class {
	#name : 'ClapMatchedValidation',
	#superclass : 'ClapValidation',
	#category : 'Clap-Core-Validation',
	#package : 'Clap-Core',
	#tag : 'Validation'
}

{ #category : 'accessing' }
ClapMatchedValidation >> description [
	^ 'Unmatched arguments'
]

{ #category : 'validation' }
ClapMatchedValidation >> failureDescriptionFor: anObject [
	^ 'Not matched!'
]

{ #category : 'testing' }
ClapMatchedValidation >> matches: aClapContext [
	^ aClapContext 
		ifMatch: [ true ] 
		ifMismatch: [ false ] 
]
