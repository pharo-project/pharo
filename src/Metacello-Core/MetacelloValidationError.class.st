"
MetacelloValidationError indicates that errors are to be expected if an attempt to use the configuration/version is made
"
Class {
	#name : #MetacelloValidationError,
	#superclass : #MetacelloValidationIssue,
	#category : #'Metacello-Core-Validation'
}

{ #category : #testing }
MetacelloValidationError >> isError [

	^true
]

{ #category : #accessing }
MetacelloValidationError >> label [

	^'Error'
]
