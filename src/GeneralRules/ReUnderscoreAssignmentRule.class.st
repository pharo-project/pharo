"
Replace _ by :=
"
Class {
	#name : #ReUnderscoreAssignmentRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReUnderscoreAssignmentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnderscoreAssignmentRule'
]

{ #category : #hooks }
ReUnderscoreAssignmentRule >> afterCheck: aNode mappings: mappingDict [
	^ aNode assignmentOperator = '_'
]

{ #category : #accessing }
ReUnderscoreAssignmentRule >> group [
	^ 'Style'
]

{ #category : #initialization }
ReUnderscoreAssignmentRule >> initialize [
	super initialize.
	self 
		replace: '`var := ``@object'
		with:    '`var := ``@object'
		"yes, they are the same"
		"see afterCheck"
]

{ #category : #accessing }
ReUnderscoreAssignmentRule >> name [
	^ 'Underscore assignements should be avoided'
]
