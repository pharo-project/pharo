"
I am the result of applying a validation to a given subject.
If the subject does not pass my criterium, I can provide a helpful description why.
"
Class {
	#name : #ClapValidationDiagnostic,
	#superclass : #Object,
	#instVars : [
		'validation',
		'subject',
		'success'
	],
	#category : #'Clap-Core-Validation'
}

{ #category : #'instance creation' }
ClapValidationDiagnostic class >> of: aValidation subject: anExpression [
	^ self new
		validation: aValidation subject: anExpression;
		yourself
]

{ #category : #accessing }
ClapValidationDiagnostic >> description [
	^ validation failureDescriptionFor: subject
]

{ #category : #testing }
ClapValidationDiagnostic >> isFailure [
	^ success not
]

{ #category : #testing }
ClapValidationDiagnostic >> isSuccess [
	^ success
]

{ #category : #printing }
ClapValidationDiagnostic >> printOn: aStream [
	aStream nextPutAll: self description
]

{ #category : #initialization }
ClapValidationDiagnostic >> validation: aValidation subject: anExpression [
	validation := aValidation.
	subject := anExpression.
	success := validation matches: subject

]
