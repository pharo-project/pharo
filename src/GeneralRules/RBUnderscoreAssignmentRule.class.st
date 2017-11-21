"
Replace _ by :=
"
Class {
	#name : #RBUnderscoreAssignmentRule,
	#superclass : #ReNodeRewriteRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBUnderscoreAssignmentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnderscoreAssignmentRule'
]

{ #category : #hooks }
RBUnderscoreAssignmentRule >> afterCheck: aNode mappings: mappingDict [
	^ aNode assignmentOperator = '_'
]

{ #category : #accessing }
RBUnderscoreAssignmentRule >> group [
	^ 'Style'
]

{ #category : #initialization }
RBUnderscoreAssignmentRule >> initialize [
	super initialize.
	self 
		replace: '`var := ``@object'
		with:    '`var := ``@object'
		"yes, they are the same"
		"see afterCheck"
]

{ #category : #accessing }
RBUnderscoreAssignmentRule >> name [
	^ 'Underscore assignements should be avoided'
]
