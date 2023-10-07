"
MetacelloValidationWarning indicates that there is a logical inconsistency that is not likely to cause any functional problems
"
Class {
	#name : 'MetacelloValidationWarning',
	#superclass : 'MetacelloValidationIssue',
	#category : 'Metacello-Core-Validation',
	#package : 'Metacello-Core',
	#tag : 'Validation'
}

{ #category : 'testing' }
MetacelloValidationWarning >> isWarning [

	^true
]

{ #category : 'accessing' }
MetacelloValidationWarning >> label [

	^'Warning'
]
