Class {
	#name : #ClapValidation,
	#superclass : #Object,
	#category : #'Clap-Core-Validation'
}

{ #category : #validation }
ClapValidation >> description [
	^ self subclassResponsibility
]

{ #category : #validation }
ClapValidation >> failureDescriptionFor: anObject [
	^ String streamContents: [ :str |
		str nextPutAll: 'Unrecognized arguments: '.
		anObject context leftovers
			do: [ :each | str nextPutAll: each ]
			separatedBy: [ str nextPutAll: ', ' ] ]
]

{ #category : #validation }
ClapValidation >> matches: aClapExpression [
	^ self subclassResponsibility
]

{ #category : #validation }
ClapValidation >> validate: anObject [
	^ ClapValidationDiagnostic of: self subject: anObject
]
