"
I represent a validation criterium that various subjects might have to pass at a later point.
My subclasses implement specific validations for Clap parameters.
"
Class {
	#name : 'ClapValidation',
	#superclass : 'Object',
	#category : 'Clap-Core-Validation',
	#package : 'Clap-Core',
	#tag : 'Validation'
}

{ #category : 'accessing' }
ClapValidation >> description [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ClapValidation >> failureDescriptionFor: anObject [
	self subclassResponsibility
]

{ #category : 'validation' }
ClapValidation >> isValidCompositeMatch: aClapCompositeMatch [ 
	^ true
]

{ #category : 'validation' }
ClapValidation >> isValidContext: aClapContext [
	^ true
]

{ #category : 'validation' }
ClapValidation >> isValidImplicit: aClapImplicit [
	^ true
]

{ #category : 'validation' }
ClapValidation >> isValidMismatch: aClapMisatch [ 
	^ false
]

{ #category : 'validation' }
ClapValidation >> isValidNamedMatch: aClapNamedMatch [ 
	^ true
]

{ #category : 'validation' }
ClapValidation >> isValidWordMatch: aClapWordMatch [ 
	^ true
]

{ #category : 'validation' }
ClapValidation >> matches: aClapExpression [

	^ aClapExpression isValid: self
]

{ #category : 'validation' }
ClapValidation >> validate: anObject [
	^ ClapValidationDiagnostic of: self subject: anObject
]
