"
Checks for assignements to temporaries that are not used afterwards.
"
Class {
	#name : #RBUnnecessaryAssignmentRule,
	#superclass : #ReNodeBasedRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBUnnecessaryAssignmentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnnecessaryAssignmentRule'
]

{ #category : #running }
RBUnnecessaryAssignmentRule >> check: aNode forCritiquesDo: aCriticBlock [
	
	aNode isReturn ifFalse: [ ^ self ].
	aNode isAssignment ifFalse: [ ^ self ].
	(aNode whoDefines: aNode variable name) ifNil: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: aNode)
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
