"
Checks for assignements to temporaries that are not used afterwards.
"
Class {
	#name : #ReUnnecessaryAssignmentRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReUnnecessaryAssignmentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnnecessaryAssignmentRule'
]

{ #category : #running }
ReUnnecessaryAssignmentRule >> basicCheck: aNode [
	aNode isReturn ifFalse: [ ^ false ].
	aNode isAssignment ifFalse: [ ^ false ].
	^ (aNode whoDefines: aNode variable name) isNotNil
]

{ #category : #accessing }
ReUnnecessaryAssignmentRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReUnnecessaryAssignmentRule >> name [
	^ 'Unnecessary assignment to a temporary variable'
]

{ #category : #accessing }
ReUnnecessaryAssignmentRule >> severity [
	^ #information
]
