"
Replace _ by :=
"
Class {
	#name : #RBUnderscoreAssignmentRule,
	#superclass : #RBTransformationRule,
	#category : #Refactoring-Critics-TransformationRules
}

{ #category : #accessing }
RBUnderscoreAssignmentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnderscoreAssignmentRule'
]

{ #category : #accessing }
RBUnderscoreAssignmentRule >> group [
	^ 'Style'
]

{ #category : #initialization }
RBUnderscoreAssignmentRule >> initialize [
	super initialize.
	self rewriteRule 
		replace: '`var := ``@object' with: '`var := ``@object'
		when: [ :node | node assignmentOperator = '_' ]
]

{ #category : #accessing }
RBUnderscoreAssignmentRule >> name [
	^ 'Underscore assignements should be avoided'
]
