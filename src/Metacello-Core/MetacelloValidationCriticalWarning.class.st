"
MetacelloValidationCriticalWarning indicates that there is a logical inconsistency that may not be intentional and that could cause incorrect loads
"
Class {
	#name : #MetacelloValidationCriticalWarning,
	#superclass : #MetacelloValidationIssue,
	#category : #'Metacello-Core-Validation'
}

{ #category : #testing }
MetacelloValidationCriticalWarning >> isCriticalWarning [

	^true
]

{ #category : #accessing }
MetacelloValidationCriticalWarning >> label [

	^'Critical Warning'
]
