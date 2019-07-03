"
Checks for assignements to temporaries that are not used afterwards.
"
Class {
	#name : #RBUnnecessaryAssignmentRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBUnnecessaryAssignmentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnnecessaryAssignmentRule'
]

{ #category : #running }
RBUnnecessaryAssignmentRule >> basicCheck: aNode [
	aNode isReturn ifFalse: [ ^ false ].
	aNode isAssignment ifFalse: [ ^ false ].
	^ (aNode whoDefines: aNode variable name) isNotNil
]

{ #category : #accessing }
RBUnnecessaryAssignmentRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
RBUnnecessaryAssignmentRule >> name [
	^ 'Unnecessary assignment to a temporary variable'
]

{ #category : #accessing }
RBUnnecessaryAssignmentRule >> severity [
	^ #information
]
